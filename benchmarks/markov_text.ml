module Circular:sig
  type !'a t
  val len: 'a t -> int
  val make: int -> 'a -> 'a t
  val from_list: 'a list -> 'a t
  val add: 'a t -> 'a ->  unit
  val (.%()): 'a t -> int -> 'a
  val pp: (Format.formatter -> 'a -> unit)-> Format.formatter -> 'a t -> unit
end = struct
  type 'a t = { mutable pos: int; buffer: 'a array }
  let len x = Array.length x.buffer
  let from_array buffer = { pos = 0; buffer = buffer }
  let from_list l = from_array (Array.of_list l)
  let make n x = { pos = 0; buffer = Array.make n x }
  let (.%()) b n = b.buffer.( (b.pos + n) mod Array.length b.buffer)
  let add b x=
    b.buffer.(b.pos) <- x;
    b.pos <- (b.pos + 1) mod Array.length b.buffer
  let pp pp_elt ppf x =
    let comma ppf  = Format.fprintf ppf ",@ " in
    Format.fprintf ppf "(";
    for i = 0 to len x - 2 do
      Format.fprintf ppf "%a%t" pp_elt x.%(i) comma
    done;
    Format.fprintf ppf "%a)" pp_elt x.%(len x - 1)
end

type ('a,'b) row = { mutable counter:'b; data:'a }

let choose array =
  let rec search f array inf sup =
    let len = Array.length array in
    if inf = len then inf - 1
    else if inf = len - 1 then inf
    else if array.(inf + 1).counter >= f then inf
    else
      let mid = inf + (sup - inf)/ 2 in
      if array.(mid).counter >= f then
        search f array inf mid
      else search f array mid sup
  in
  let index = search (Random.float 1.0) array 0 (Array.length array) in
  array.(index).data

module Linked_tables(Container: sig type 'a t end) = struct
  type ('a,'b) link = End of ('a,'b) t option ref | Table of ('a,'b) t
  and ('a,'b) r = ('a * ('a,'b) link, 'b) row
  and ('a,'b) t = ('a,'b) r Container.t
end


module String_map = Map.Make(String)

module Array_tables = Linked_tables(Array)
module Input_tables = Linked_tables(struct type 'a t = 'a String_map.t ref end)

let rec first_ngram tables =
  let name, after = choose tables in
  match after with
  | Array_tables.End _ -> [name]
  | Table tables ->
    name :: first_ngram tables

exception Dead_end

let find_name name table =
  let rec search name table inf sup =
    let mid = inf + (sup - inf)/2 in
    if mid = inf then raise Dead_end else
    let k, t = table.(mid).data in
    if k > name then
      search name table inf mid
    else if k = name then
      t
    else
      search name table mid sup
  in
  search name table 0 (Array.length table)

let next tables ngram link =
  let rec subtable ngram pos perfect table =
    if pos = Circular.len ngram then
      table, perfect
    else
      match find_name ngram.Circular.%(pos) table with
      | Array_tables.End _ -> assert false
      | Table t -> subtable ngram (pos+1) perfect t
      | exception Dead_end ->
        let _, t = choose table in
        begin
          match t with
          | Array_tables.End _ -> assert false
          | Array_tables.Table t -> subtable ngram (pos+1) false t
        end
  in
  let table = match !link with
    | None ->
      let table, perfect = subtable ngram 1 true tables in
      if perfect then link := Some table;
      table
    | Some l -> l
  in
  let name, t = choose table in
  let link = match t with
    | Array_tables.End link -> link
    | Array_tables.Table t ->
      Format.eprintf "@[name=%s, children=%a@]@."
        name
        (Format.pp_print_seq ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (fun ppf r -> Format.fprintf ppf "%s" (fst r.data))
        ) (Array.to_seq t);
      assert false
  in
  Circular.add ngram name;
  name, link

let write table n =
  let b = Buffer.create 1000 in
  let ngram = Circular.from_list @@ first_ngram table in
  for i = 0 to Circular.len ngram - 1 do
    Buffer.add_string b ngram.Circular.%(i)
  done;
  let rec write b table n link =
    if n <= 0 then () else
      let name, link = next table ngram link in
      Buffer.add_string b name;
      write b table (n-1) link
  in
  write b table n


let rec normalize_link htables = match htables with
  | Input_tables.End _ -> Array_tables.End (ref None)
  | Table c -> Array_tables.Table (normalize !c)
and normalize c  =
    let count = float @@ String_map.fold (fun _ x acc -> x.counter + acc) c 0 in
    let array = Array.make (String_map.cardinal c) ({ counter = 0.; data = "", Array_tables.End (ref None)}) in
    let _, _ = String_map.fold (fun _k x (i, ic) ->
        let name, tables = x.data in
        let tables = normalize_link tables in
        let data = name, tables in
        let entry = { counter = float ic /. count; data } in
        let ic = ic + x.counter in
        array.(i) <- entry;
        i + 1, ic
      ) c (0,0)
    in
    array


exception Dim_error


let rec chain pos ngram: (string,int) Input_tables.r =
  let name = ngram.Circular.%(pos) in
  if pos = Circular.len ngram - 1 then
    { counter = 1; data= (name, Input_tables.End (ref None)) }
  else
    let chain = chain (pos+1) ngram in
    { counter = 1; data= (name, Input_tables.Table (ref @@ String_map.singleton name chain) ) }

let read table ngram =
  let rec read (table: (string,int) Input_tables.t) ngram pos =
    if pos = Circular.len ngram then ()
    else
      let name = ngram.Circular.%(pos) in
      match String_map.find_opt name !table with
      | None ->
        let subtable = chain pos ngram in
        table := String_map.add name subtable !table
      | Some ({ data = _, Input_tables.End _ ; _ } as r) ->
        if pos = Circular.len ngram - 1 then
          r.counter <- r.counter + 1
        else (
          Format.eprintf "@[ngram=%a (%d)@]@." (Circular.pp (fun ppf -> Format.fprintf ppf "%S")) ngram pos;
          raise Dim_error
        )
      | Some ({ data = _name, Input_tables.Table c; counter } as r)  ->
        r.counter <- counter + 1;
        read c ngram (pos+1)
  in
  read table ngram 0

type 'a feeder =
  | Incomplete of int * 'a list
  | Complete of 'a Circular.t * (string,int) Input_tables.t

let read_text n reader =
  let feed status word =
    let word = String.trim word in
    if word = "" then status else
    match status with
    | Incomplete (n,l) ->
      let l = word :: l in
      if n = 1 then
        let ngram = Circular.from_list (List.rev l) in
        let tables = chain 0 ngram in
        Complete  (ngram, ref (String_map.singleton ngram.Circular.%(0) tables))
      else
        Incomplete (n-1, l)
    | Complete (ngram,t) as c ->
      Circular.add ngram word;
      read t ngram;
      c
  in
  reader feed (Incomplete (n, []))

type arg = { parameters: Type.parameters; ngram: string Circular.t; tables: (string,float) Array_tables.t }

let test={| a a a|}

let text = {|
Hwæt. We Gardena in geardagum,
þeodcyninga, þrym gefrunon,
hu ða æþelingas ellen fremedon.
Oft Scyld Scefing sceaþena þreatum,
monegum mægþum, meodosetla ofteah,
egsode eorlas. Syððan ærest wearð
feasceaft funden, he þæs frofre gebad,
weox under wolcnum, weorðmyndum þah,
oðþæt him æghwylc þara ymbsittendra
ofer hronrade hyran scolde,
gomban gyldan. þæt wæs god cyning.
ðæm eafera wæs æfter cenned,
geong in geardum, þone god sende
folce to frofre; fyrenðearfe ongeat
þe hie ær drugon aldorlease
lange hwile. Him þæs liffrea,
wuldres wealdend, woroldare forgeaf;
Beowulf wæs breme blæd wide sprang,
Scyldes eafera Scedelandum in.
Swa sceal geong guma gode gewyrcean,
fromum feohgiftum on fæder bearme,
þæt hine on ylde eft gewunigen
wilgesiþas, þonne wig cume,
leode gelæsten; lofdædum sceal
in mægþa gehwære man geþeon.
Him ða Scyld gewat to gescæphwile
felahror feran on frean wære.
Hi hyne þa ætbæron to brimes faroðe,
swæse gesiþas, swa he selfa bæd,
þenden wordum weold wine Scyldinga;
leof landfruma lange ahte.
þær æt hyðe stod hringedstefna,
isig ond utfus, æþelinges fær.
Aledon þa leofne þeoden,
beaga bryttan, on bearm scipes,
mærne be mæste. þær wæs madma fela
of feorwegum, frætwa, gelæded;
ne hyrde ic cymlicor ceol gegyrwan
hildewæpnum ond heaðowædum,
billum ond byrnum; him on bearme læg
madma mænigo, þa him mid scoldon
on flodes æht feor gewitan.
Nalæs hi hine læssan lacum teodan,
þeodgestreonum, þon þa dydon
þe hine æt frumsceafte forð onsendon
ænne ofer yðe umborwesende.
þa gyt hie him asetton segen geldenne
heah ofer heafod, leton holm beran,
geafon on garsecg; him wæs geomor sefa,
murnende mod. Men ne cunnon
secgan to soðe, selerædende,
hæleð under heofenum, hwa þæm hlæste onfeng.
ða wæs on burgum Beowulf Scyldinga,
leof leodcyning, longe þrage
folcum gefræge fæder ellor hwearf,
aldor of earde, oþþæt him eft onwoc
heah Healfdene; heold þenden lifde,
gamol ond guðreouw, glæde Scyldingas.
ðæm feower bearn forð gerimed
in worold wocun, weoroda ræswan,
Heorogar ond Hroðgar ond Halga til;
hyrde ic þæt wæs Onelan cwen,
Heaðoscilfingas healsgebedda.
þa wæs Hroðgare heresped gyfen,
wiges weorðmynd, þæt him his winemagas
georne hyrdon, oðð þæt seo geogoð geweox,
magodriht micel. Him on mod bearn
þæt healreced hatan wolde,
medoærn micel, men gewyrcean
þonne yldo bearn æfre gefrunon,
ond þær on innan eall gedælan
geongum ond ealdum, swylc him god sealde,
buton folcscare ond feorum gumena.
ða ic wide gefrægn weorc gebannan
manigre mægþe geond þisne middangeard,
folcstede frætwan. Him on fyrste gelomp,
ædre mid yldum, þæt hit wearð ealgearo,
healærna mæst; scop him Heort naman
se þe his wordes geweald wide hæfde.
He beot ne aleh, beagas dælde,
sinc æt symle. Sele hlifade,
heah ond horngeap, heaðowylma bad,
laðan liges; ne wæs hit lenge þa gen
þæt se ecghete aþumsweorum,
æfter wælniðe wæcnan scolde.
ða se ellengæst earfoðlice
þrage geþolode, se þe in þystrum bad,
þæt he dogora gehwam dream gehyrde
hludne in healle; þær wæs hearpan sweg,
swutol sang scopes. Sægde se þe cuþe
frumsceaft fira feorran reccan,
cwæð þæt se ælmihtiga eorðan worhte,
wlitebeorhtne wang, swa wæter bebugeð,
gesette sigehreþig sunnan ond monan
leoman to leohte landbuendum
ond gefrætwade foldan sceatas
leomum ond leafum, lif eac gesceop
cynna gehwylcum þara ðe cwice hwyrfaþ.
Swa ða drihtguman dreamum lifdon
eadiglice, oððæt an ongan
fyrene fremman feond on helle.
Wæs se grimma gæst Grendel haten,
mære mearcstapa, se þe moras heold,
fen ond fæsten; fifelcynnes eard
wonsæli wer weardode hwile,
siþðan him scyppend forscrifen hæfde
in Caines cynne. þone cwealm gewræc
ece drihten, þæs þe he Abel slog;
ne gefeah he þære fæhðe, ac he hine feor forwræc,
metod for þy mane, mancynne fram.
þanon untydras ealle onwocon,
eotenas ond ylfe ond orcneas,
swylce gigantas, þa wið gode wunnon
lange þrage; he him ðæs lean forgeald.
Gewat ða neosian, syþðan niht becom,
hean huses, hu hit Hringdene
æfter beorþege gebun hæfdon.
Fand þa ðær inne æþelinga gedriht
swefan æfter symble; sorge ne cuðon,
wonsceaft wera. Wiht unhælo,
grim ond grædig, gearo sona wæs,
reoc ond reþe, ond on ræste genam
þritig þegna, þanon eft gewat
huðe hremig to ham faran,
mid þære wælfylle wica neosan.
ða wæs on uhtan mid ærdæge
Grendles guðcræft gumum undyrne;
þa wæs æfter wiste wop up ahafen,
micel morgensweg. Mære þeoden,
æþeling ærgod, unbliðe sæt,
þolode ðryðswyð, þegnsorge dreah,
syðþan hie þæs laðan last sceawedon,
wergan gastes; wæs þæt gewin to strang,
lað ond longsum. Næs hit lengra fyrst,
ac ymb ane niht eft gefremede
morðbeala mare ond no mearn fore,
fæhðe ond fyrene; wæs to fæst on þam.
þa wæs eaðfynde þe him elles hwær
gerumlicor ræste sohte,
bed æfter burum, ða him gebeacnod wæs,
gesægd soðlice sweotolan tacne
healðegnes hete; heold hyne syðþan
fyr ond fæstor se þæm feonde ætwand.
Swa rixode ond wið rihte wan,
ana wið eallum, oðþæt idel stod
husa selest. Wæs seo hwil micel;
XII wintra tid torn geþolode
wine Scyldinga, weana gehwelcne,
sidra sorga. Forðam secgum wearð,
ylda bearnum, undyrne cuð,
gyddum geomore, þætte Grendel wan
hwile wið Hroþgar, heteniðas wæg,
fyrene ond fæhðe fela missera,
singale sæce, sibbe ne wolde
wið manna hwone mægenes Deniga,
feorhbealo feorran, fea þingian,
ne þær nænig witena wenan þorfte
beorhtre bote to banan folmum,
ac se æglæca ehtende wæs,
deorc deaþscua, duguþe ond geogoþe,
seomade ond syrede, sinnihte heold
mistige moras. men ne cunnon
hwyder helrunan hwyrftum scriþað.
Swa fela fyrena feond mancynnes,
atol angengea, oft gefremede,
heardra hynða. Heorot eardode,
sincfage sel sweartum nihtum;
no he þone gifstol gretan moste,
maþðum for metode, ne his myne wisse.
þæt wæs wræc micel wine Scyldinga,
modes brecða. Monig oft gesæt
rice to rune; ræd eahtedon
hwæt swiðferhðum selest wære
wið færgryrum to gefremmanne.
Hwilum hie geheton æt hærgtrafum
wigweorþunga, wordum bædon
þæt him gastbona geoce gefremede
wið þeodþreaum. Swylc wæs þeaw hyra,
hæþenra hyht; helle gemundon
in modsefan, metod hie ne cuþon,
dæda demend, ne wiston hie drihten god,
ne hie huru heofena helm herian ne cuþon,
wuldres waldend. Wa bið þæm ðe sceal
þurh sliðne nið sawle bescufan
in fyres fæþm, frofre ne wenan,
wihte gewendan; wel bið þæm þe mot
æfter deaðdæge drihten secean
ond to fæder fæþmum freoðo wilnian.
Swa ða mælceare maga Healfdenes
singala seað, ne mihte snotor hæleð
wean onwendan; wæs þæt gewin to swyð,
laþ ond longsum, þe on ða leode becom,
nydwracu niþgrim, nihtbealwa mæst.
þæt fram ham gefrægn Higelaces þegn,
god mid Geatum, Grendles dæda;
se wæs moncynnes mægenes strengest
on þæm dæge þysses lifes,
æþele ond eacen. Het him yðlidan
godne gegyrwan, cwæð, hu guðcyning
ofer swanrade secean wolde,
mærne þeoden, þa him wæs manna þearf.
ðone siðfæt him snotere ceorlas
lythwon logon, þeah he him leof wære;
hwetton higerofne, hæl sceawedon.
Hæfde se goda Geata leoda
cempan gecorone þara þe he cenoste
findan mihte; XVna sum
sundwudu sohte; secg wisade,
lagucræftig mon, landgemyrcu.
Fyrst forð gewat. Flota wæs on yðum,
bat under beorge. Beornas gearwe
on stefn stigon; streamas wundon,
sund wið sande; secgas bæron
on bearm nacan beorhte frætwe,
guðsearo geatolic; guman ut scufon,
weras on wilsið, wudu bundenne.
Gewat þa ofer wægholm, winde gefysed,
flota famiheals fugle gelicost,
oðþæt ymb antid oþres dogores
wundenstefna gewaden hæfde
þæt ða liðende land gesawon,
brimclifu blican, beorgas steape,
side sænæssas; þa wæs sund liden,
eoletes æt ende. þanon up hraðe
Wedera leode on wang stigon,
sæwudu sældon syrcan hrysedon,
guðgewædo, gode þancedon
þæs þe him yþlade eaðe wurdon.
þa of wealle geseah weard Scildinga,
se þe holmclifu healdan scolde,
beran ofer bolcan beorhte randas,
fyrdsearu fuslicu; hine fyrwyt bræc
modgehygdum, hwæt þa men wæron.
Gewat him þa to waroðe wicge ridan
þegn Hroðgares, þrymmum cwehte
mægenwudu mundum, meþelwordum frægn:
Hwæt syndon ge searohæbbendra,
byrnum werede, þe þus brontne ceol
ofer lagustræte lædan cwomon,
hider ofer holmas? le wæs
endesæta, ægwearde heold,
þe on land Dena laðra nænig
mid scipherge sceðþan ne meahte.
No her cuðlicor cuman ongunnon
lindhæbbende; ne ge leafnesword
guðfremmendra gearwe ne wisson,
maga gemedu. Næfre ic maran geseah
eorla ofer eorþan ðonne is eower sum,
secg on searwum; nis þæt seldguma,
wæpnum geweorðad, næfne him his wlite leoge,
ænlic ansyn. Nu ic eower sceal
frumcyn witan, ær ge fyr heonan ,
leassceaweras, on land Dena
furþur feran. Nu ge feorbuend,
mereliðende, minne gehyrað
anfealdne geþoht: Ofost is selest
to gecyðanne hwanan eowre cyme syndon.
Him se yldesta ondswarode,
werodes wisa, wordhord onleac:
We synt gumcynnes Geata leode
ond Higelaces heorðgeneatas.
Wæs min fæder folcum gecyþed,
æþele ordfruma, Ecgþeow haten.
Gebad wintra worn, ær he on weg hwurfe,
gamol of geardum; hine gearwe geman
witena welhwylc wide geond eorþan.
We þurh holdne hige hlaford þinne,
sunu Healfdenes, secean cwomon,
leodgebyrgean; wes þu us larena god.
Habbað we to þæm mæran micel ærende,
Deniga frean, ne sceal þær dyrne sum
wesan, þæs ic wene. þu wast gif hit is
swa we soþlice secgan hyrdon
þæt mid Scyldingum sceaðona ic nat hwylc,
deogol dædhata, deorcum nihtum
eaweð þurh egsan uncuðne nið,
hynðu ond hrafyl. Ic þæs Hroðgar mæg
þurh rumne sefan ræd gelæran,
hu he frod ond god feond oferswyðeþ,
gyf him edwendan æfre scolde
bealuwa bisigu, bot eft cuman,
ond þa cearwylmas colran wurðaþ;
oððe a syþðan earfoðþrage,
þreanyd þolað, þenden þær wunað
on heahstede husa selest.
Weard maþelode, ðær on wicge sæt,
ombeht unforht: æghwæþres sceal
scearp scyldwiga gescad witan,
worda ond worca, se þe wel þenceð.
Ic þæt gehyre, þæt þis is hold weorod
frean Scyldinga. Gewitaþ forð beran
wæpen ond gewædu; ic eow wisige.
Swylce ic maguþegnas mine hate
wið feonda gehwone flotan eowerne,
niwtyrwydne nacan on sande
arum healdan, oþðæt eft byreð
ofer lagustreamas leofne mannan
wudu wundenhals to Wedermearce,
godfremmendra swylcum gifeþe bið
þæt þone hilderæs hal gedigeð.
Gewiton him þa feran. Flota stille bad,
seomode on sale sidfæþmed scip,
on ancre fæst. Eoforlic scionon
ofer hleorberan gehroden golde,
fah ond fyrheard; ferhwearde heold
guþmod grimmon. Guman onetton,
sigon ætsomne, oþþæt hy sæl timbred,
geatolic ond goldfah, ongyton mihton;
þæt wæs foremærost foldbuendum
receda under roderum, on þæm se rica bad;
lixte se leoma ofer landa fela.
Him þa hildedeor hof modigra
torht getæhte, þæt hie him to mihton
gegnum gangan; guðbeorna sum
wicg gewende, word æfter cwæð:
Mæl is me to feran; fæder alwalda
mid arstafum eowic gehealde
siða gesunde. Ic to sæ wille
wið wrað werod wearde healdan.
Stræt wæs stanfah, stig wisode
gumum ætgædere. Guðbyrne scan
heard hondlocen, hringiren scir
song in searwum, þa hie to sele furðum
in hyra gryregeatwum gangan cwomon.
Setton sæmeþe side scyldas,
rondas regnhearde, wið þæs recedes weal,
bugon þa to bence. Byrnan hringdon,
guðsearo gumena; garas stodon,
sæmanna searo, samod ætgædere,
æscholt ufan græg; wæs se irenþreat
wæpnum gewurþad. þa ðær wlonc hæleð
oretmecgas æfter æþelum frægn:
Hwanon ferigeað ge fætte scyldas,
græge syrcan ond grimhelmas,
heresceafta heap? Ic eom Hroðgares
ar ond ombiht. Ne seah ic elþeodige
þus manige men modiglicran.
Wen ic þæt ge for wlenco, nalles for wræcsiðum,
ac for higeþrymmum Hroðgar sohton.
Him þa ellenrof andswarode,
wlanc Wedera leod, word æfter spræc,
heard under helme: We synt Higelaces
beodgeneatas; Beowulf is min nama.
Wille ic asecgan sunu Healfdenes,
mærum þeodne, min ærende,
aldre þinum, gif he us geunnan wile
þæt we hine swa godne gretan moton.
Wulfgar maþelode þæt wæs Wendla leod;
wæs his modsefa manegum gecyðed,
wig ond wisdom: Ic þæs wine Deniga,
frean Scildinga, frinan wille,
beaga bryttan, swa þu bena eart,
þeoden mærne, ymb þinne sið,
ond þe þa ondsware ædre gecyðan
ðe me se goda agifan þenceð.
Hwearf þa hrædlice þær Hroðgar sæt
eald ond anhar mid his eorla gedriht;
eode ellenrof, þæt he for eaxlum gestod
Deniga frean; cuþe he duguðe þeaw.
Wulfgar maðelode to his winedrihtne:
Her syndon geferede, feorran cumene
ofer geofenes begang Geata leode;
þone yldestan oretmecgas
Beowulf nemnað. Hy benan synt
þæt hie, þeoden min, wið þe moton
wordum wrixlan. No ðu him wearne geteoh
ðinra gegncwida, glædman Hroðgar.
Hy on wiggetawum wyrðe þinceað
eorla geæhtlan; huru se aldor deah,
se þæm heaðorincum hider wisade.
Hroðgar maþelode, helm Scyldinga:
Ic hine cuðe cnihtwesende.
Wæs his ealdfæder Ecgþeo haten,
ðæm to ham forgeaf Hreþel Geata
angan dohtor; is his eafora nu
heard her cumen, sohte holdne wine.
ðonne sægdon þæt sæliþende,
þa ðe gifsceattas Geata fyredon
þyder to þance, þæt he XXXtiges
manna mægencræft on his mundgripe
heaþorof hæbbe. Hine halig god
for arstafum us onsende,
to Westdenum, þæs ic wen hæbbe,
wið Grendles gryre. Ic þæm godan sceal
for his modþræce madmas beodan.
Beo ðu on ofeste, hat in gan
seon sibbegedriht samod ætgædere;
gesaga him eac wordum þæt hie sint wilcuman
Deniga leodum.
[] word inne abead:
Eow het secgan sigedrihten min,
aldor Eastdena, þæt he eower æþelu can,
ond ge him syndon ofer sæwylmas
heardhicgende hider wilcuman.
Nu ge moton gangan in eowrum guðgeatawum
under heregriman Hroðgar geseon;
lætað hildebord her onbidan,
wudu, wælsceaftas, worda geþinges.
Aras þa se rica, ymb hine rinc manig,
þryðlic þegna heap; sume þær bidon,
heaðoreaf heoldon, swa him se hearda bebead.
Snyredon ætsomne, þa secg wisode,
under Heorotes hrof
heard under helme, þæt he on heoðe gestod.
Beowulf maðelode on him byrne scan,
searonet seowed smiþes orþancum:
Wæs þu, Hroðgar, hal. Ic eom Higelaces
mæg ond magoðegn; hæbbe ic mærða fela
ongunnen on geogoþe. Me wearð Grendles þing
on minre eþeltyrf undyrne cuð;
secgað sæliðend þæt þæs sele stande,
reced selesta, rinca gehwylcum
idel ond unnyt, siððan æfenleoht
under heofenes hador beholen weorþeð.
þa me þæt gelærdon leode mine
þa selestan, snotere ceorlas,
þeoden Hroðgar, þæt ic þe sohte,
forþan hie mægenes cræft minne cuþon,
selfe ofersawon, ða ic of searwum cwom,
fah from feondum. þær ic fife geband,
yðde eotena cyn ond on yðum slog
niceras nihtes, nearoþearfe dreah,
wræc Wedera nið wean ahsodon,
forgrand gramum, ond nu wið Grendel sceal,
wið þam aglæcan, ana gehegan
ðing wið þyrse. Ic þe nu ða,
brego Beorhtdena, biddan wille,
eodor Scyldinga, anre bene,
þæt ðu me ne forwyrne, wigendra hleo,
freowine folca, nu ic þus feorran com,
þæt ic mote ana ond minra eorla gedryht,
þes hearda heap, Heorot fælsian.
Hæbbe ic eac geahsod þæt se æglæca
for his wonhydum wæpna ne recceð.
Ic þæt þonne forhicge swa me Higelac sie,
min mondrihten, modes bliðe,
þæt ic sweord bere oþðe sidne scyld,
geolorand to guþe, ac ic mid grape sceal
fon wið feonde ond ymb feorh sacan,
lað wið laþum; ðær gelyfan sceal
dryhtnes dome se þe hine deað nimeð.
Wen ic þæt he wille, gif he wealdan mot,
in þæm guðsele Geotena leode
etan unforhte, swa he oft dyde,
mægen Hreðmanna. Na þu minne þearft
hafalan hydan, ac he me habban wile
dreore fahne, gif mec deað nimeð.
Byreð blodig wæl, byrgean þenceð,
eteð angenga unmurnlice,
mearcað morhopu; no ðu ymb mines ne þearft
lices feorme leng sorgian.
|}

let init parameters =
  let fold f acc =
    Uuseg_string.fold_utf_8 `Word f acc text
  in
  match read_text 2 fold with
  | Incomplete _ -> assert false
  | Complete(_, t) ->
    let t = normalize !t in
    let ngram = Circular.from_list (first_ngram t) in
    {parameters; ngram; tables = t }

let run {parameters; ngram; tables = t } =
  let rec word b n t ngram link =
    if n = 0 then ()
    else
      let name, link = next t ngram link in
      Buffer.add_string b name;
      Buffer.add_char b ' ';
      word b (n-1) t ngram link
  in
  let b = Buffer.create 1000 in
  word b parameters.Type.size t ngram (ref None);
  print_string (Buffer.contents b)

let benchmark = Type.Benchmark {init;run}
