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
Hw??t. We Gardena in geardagum,
??eodcyninga, ??rym gefrunon,
hu ??a ????elingas ellen fremedon.
Oft Scyld Scefing scea??ena ??reatum,
monegum m??g??um, meodosetla ofteah,
egsode eorlas. Sy????an ??rest wear??
feasceaft funden, he ????s frofre gebad,
weox under wolcnum, weor??myndum ??ah,
o??????t him ??ghwylc ??ara ymbsittendra
ofer hronrade hyran scolde,
gomban gyldan. ????t w??s god cyning.
????m eafera w??s ??fter cenned,
geong in geardum, ??one god sende
folce to frofre; fyren??earfe ongeat
??e hie ??r drugon aldorlease
lange hwile. Him ????s liffrea,
wuldres wealdend, woroldare forgeaf;
Beowulf w??s breme bl??d wide sprang,
Scyldes eafera Scedelandum in.
Swa sceal geong guma gode gewyrcean,
fromum feohgiftum on f??der bearme,
????t hine on ylde eft gewunigen
wilgesi??as, ??onne wig cume,
leode gel??sten; lofd??dum sceal
in m??g??a gehw??re man ge??eon.
Him ??a Scyld gewat to gesc??phwile
felahror feran on frean w??re.
Hi hyne ??a ??tb??ron to brimes faro??e,
sw??se gesi??as, swa he selfa b??d,
??enden wordum weold wine Scyldinga;
leof landfruma lange ahte.
????r ??t hy??e stod hringedstefna,
isig ond utfus, ????elinges f??r.
Aledon ??a leofne ??eoden,
beaga bryttan, on bearm scipes,
m??rne be m??ste. ????r w??s madma fela
of feorwegum, fr??twa, gel??ded;
ne hyrde ic cymlicor ceol gegyrwan
hildew??pnum ond hea??ow??dum,
billum ond byrnum; him on bearme l??g
madma m??nigo, ??a him mid scoldon
on flodes ??ht feor gewitan.
Nal??s hi hine l??ssan lacum teodan,
??eodgestreonum, ??on ??a dydon
??e hine ??t frumsceafte for?? onsendon
??nne ofer y??e umborwesende.
??a gyt hie him asetton segen geldenne
heah ofer heafod, leton holm beran,
geafon on garsecg; him w??s geomor sefa,
murnende mod. Men ne cunnon
secgan to so??e, seler??dende,
h??le?? under heofenum, hwa ????m hl??ste onfeng.
??a w??s on burgum Beowulf Scyldinga,
leof leodcyning, longe ??rage
folcum gefr??ge f??der ellor hwearf,
aldor of earde, o??????t him eft onwoc
heah Healfdene; heold ??enden lifde,
gamol ond gu??reouw, gl??de Scyldingas.
????m feower bearn for?? gerimed
in worold wocun, weoroda r??swan,
Heorogar ond Hro??gar ond Halga til;
hyrde ic ????t w??s Onelan cwen,
Hea??oscilfingas healsgebedda.
??a w??s Hro??gare heresped gyfen,
wiges weor??mynd, ????t him his winemagas
georne hyrdon, o???? ????t seo geogo?? geweox,
magodriht micel. Him on mod bearn
????t healreced hatan wolde,
medo??rn micel, men gewyrcean
??onne yldo bearn ??fre gefrunon,
ond ????r on innan eall ged??lan
geongum ond ealdum, swylc him god sealde,
buton folcscare ond feorum gumena.
??a ic wide gefr??gn weorc gebannan
manigre m??g??e geond ??isne middangeard,
folcstede fr??twan. Him on fyrste gelomp,
??dre mid yldum, ????t hit wear?? ealgearo,
heal??rna m??st; scop him Heort naman
se ??e his wordes geweald wide h??fde.
He beot ne aleh, beagas d??lde,
sinc ??t symle. Sele hlifade,
heah ond horngeap, hea??owylma bad,
la??an liges; ne w??s hit lenge ??a gen
????t se ecghete a??umsweorum,
??fter w??lni??e w??cnan scolde.
??a se elleng??st earfo??lice
??rage ge??olode, se ??e in ??ystrum bad,
????t he dogora gehwam dream gehyrde
hludne in healle; ????r w??s hearpan sweg,
swutol sang scopes. S??gde se ??e cu??e
frumsceaft fira feorran reccan,
cw???? ????t se ??lmihtiga eor??an worhte,
wlitebeorhtne wang, swa w??ter bebuge??,
gesette sigehre??ig sunnan ond monan
leoman to leohte landbuendum
ond gefr??twade foldan sceatas
leomum ond leafum, lif eac gesceop
cynna gehwylcum ??ara ??e cwice hwyrfa??.
Swa ??a drihtguman dreamum lifdon
eadiglice, o??????t an ongan
fyrene fremman feond on helle.
W??s se grimma g??st Grendel haten,
m??re mearcstapa, se ??e moras heold,
fen ond f??sten; fifelcynnes eard
wons??li wer weardode hwile,
si????an him scyppend forscrifen h??fde
in Caines cynne. ??one cwealm gewr??c
ece drihten, ????s ??e he Abel slog;
ne gefeah he ????re f??h??e, ac he hine feor forwr??c,
metod for ??y mane, mancynne fram.
??anon untydras ealle onwocon,
eotenas ond ylfe ond orcneas,
swylce gigantas, ??a wi?? gode wunnon
lange ??rage; he him ????s lean forgeald.
Gewat ??a neosian, sy????an niht becom,
hean huses, hu hit Hringdene
??fter beor??ege gebun h??fdon.
Fand ??a ????r inne ????elinga gedriht
swefan ??fter symble; sorge ne cu??on,
wonsceaft wera. Wiht unh??lo,
grim ond gr??dig, gearo sona w??s,
reoc ond re??e, ond on r??ste genam
??ritig ??egna, ??anon eft gewat
hu??e hremig to ham faran,
mid ????re w??lfylle wica neosan.
??a w??s on uhtan mid ??rd??ge
Grendles gu??cr??ft gumum undyrne;
??a w??s ??fter wiste wop up ahafen,
micel morgensweg. M??re ??eoden,
????eling ??rgod, unbli??e s??t,
??olode ??ry??swy??, ??egnsorge dreah,
sy????an hie ????s la??an last sceawedon,
wergan gastes; w??s ????t gewin to strang,
la?? ond longsum. N??s hit lengra fyrst,
ac ymb ane niht eft gefremede
mor??beala mare ond no mearn fore,
f??h??e ond fyrene; w??s to f??st on ??am.
??a w??s ea??fynde ??e him elles hw??r
gerumlicor r??ste sohte,
bed ??fter burum, ??a him gebeacnod w??s,
ges??gd so??lice sweotolan tacne
heal??egnes hete; heold hyne sy????an
fyr ond f??stor se ????m feonde ??twand.
Swa rixode ond wi?? rihte wan,
ana wi?? eallum, o??????t idel stod
husa selest. W??s seo hwil micel;
XII wintra tid torn ge??olode
wine Scyldinga, weana gehwelcne,
sidra sorga. For??am secgum wear??,
ylda bearnum, undyrne cu??,
gyddum geomore, ????tte Grendel wan
hwile wi?? Hro??gar, heteni??as w??g,
fyrene ond f??h??e fela missera,
singale s??ce, sibbe ne wolde
wi?? manna hwone m??genes Deniga,
feorhbealo feorran, fea ??ingian,
ne ????r n??nig witena wenan ??orfte
beorhtre bote to banan folmum,
ac se ??gl??ca ehtende w??s,
deorc dea??scua, dugu??e ond geogo??e,
seomade ond syrede, sinnihte heold
mistige moras. men ne cunnon
hwyder helrunan hwyrftum scri??a??.
Swa fela fyrena feond mancynnes,
atol angengea, oft gefremede,
heardra hyn??a. Heorot eardode,
sincfage sel sweartum nihtum;
no he ??one gifstol gretan moste,
ma????um for metode, ne his myne wisse.
????t w??s wr??c micel wine Scyldinga,
modes brec??a. Monig oft ges??t
rice to rune; r??d eahtedon
hw??t swi??ferh??um selest w??re
wi?? f??rgryrum to gefremmanne.
Hwilum hie geheton ??t h??rgtrafum
wigweor??unga, wordum b??don
????t him gastbona geoce gefremede
wi?? ??eod??reaum. Swylc w??s ??eaw hyra,
h????enra hyht; helle gemundon
in modsefan, metod hie ne cu??on,
d??da demend, ne wiston hie drihten god,
ne hie huru heofena helm herian ne cu??on,
wuldres waldend. Wa bi?? ????m ??e sceal
??urh sli??ne ni?? sawle bescufan
in fyres f????m, frofre ne wenan,
wihte gewendan; wel bi?? ????m ??e mot
??fter dea??d??ge drihten secean
ond to f??der f????mum freo??o wilnian.
Swa ??a m??lceare maga Healfdenes
singala sea??, ne mihte snotor h??le??
wean onwendan; w??s ????t gewin to swy??,
la?? ond longsum, ??e on ??a leode becom,
nydwracu ni??grim, nihtbealwa m??st.
????t fram ham gefr??gn Higelaces ??egn,
god mid Geatum, Grendles d??da;
se w??s moncynnes m??genes strengest
on ????m d??ge ??ysses lifes,
????ele ond eacen. Het him y??lidan
godne gegyrwan, cw????, hu gu??cyning
ofer swanrade secean wolde,
m??rne ??eoden, ??a him w??s manna ??earf.
??one si??f??t him snotere ceorlas
lythwon logon, ??eah he him leof w??re;
hwetton higerofne, h??l sceawedon.
H??fde se goda Geata leoda
cempan gecorone ??ara ??e he cenoste
findan mihte; XVna sum
sundwudu sohte; secg wisade,
lagucr??ftig mon, landgemyrcu.
Fyrst for?? gewat. Flota w??s on y??um,
bat under beorge. Beornas gearwe
on stefn stigon; streamas wundon,
sund wi?? sande; secgas b??ron
on bearm nacan beorhte fr??twe,
gu??searo geatolic; guman ut scufon,
weras on wilsi??, wudu bundenne.
Gewat ??a ofer w??gholm, winde gefysed,
flota famiheals fugle gelicost,
o??????t ymb antid o??res dogores
wundenstefna gewaden h??fde
????t ??a li??ende land gesawon,
brimclifu blican, beorgas steape,
side s??n??ssas; ??a w??s sund liden,
eoletes ??t ende. ??anon up hra??e
Wedera leode on wang stigon,
s??wudu s??ldon syrcan hrysedon,
gu??gew??do, gode ??ancedon
????s ??e him y??lade ea??e wurdon.
??a of wealle geseah weard Scildinga,
se ??e holmclifu healdan scolde,
beran ofer bolcan beorhte randas,
fyrdsearu fuslicu; hine fyrwyt br??c
modgehygdum, hw??t ??a men w??ron.
Gewat him ??a to waro??e wicge ridan
??egn Hro??gares, ??rymmum cwehte
m??genwudu mundum, me??elwordum fr??gn:
Hw??t syndon ge searoh??bbendra,
byrnum werede, ??e ??us brontne ceol
ofer lagustr??te l??dan cwomon,
hider ofer holmas? le w??s
endes??ta, ??gwearde heold,
??e on land Dena la??ra n??nig
mid scipherge sce????an ne meahte.
No her cu??licor cuman ongunnon
lindh??bbende; ne ge leafnesword
gu??fremmendra gearwe ne wisson,
maga gemedu. N??fre ic maran geseah
eorla ofer eor??an ??onne is eower sum,
secg on searwum; nis ????t seldguma,
w??pnum geweor??ad, n??fne him his wlite leoge,
??nlic ansyn. Nu ic eower sceal
frumcyn witan, ??r ge fyr heonan ,
leassceaweras, on land Dena
fur??ur feran. Nu ge feorbuend,
mereli??ende, minne gehyra??
anfealdne ge??oht: Ofost is selest
to gecy??anne hwanan eowre cyme syndon.
Him se yldesta ondswarode,
werodes wisa, wordhord onleac:
We synt gumcynnes Geata leode
ond Higelaces heor??geneatas.
W??s min f??der folcum gecy??ed,
????ele ordfruma, Ecg??eow haten.
Gebad wintra worn, ??r he on weg hwurfe,
gamol of geardum; hine gearwe geman
witena welhwylc wide geond eor??an.
We ??urh holdne hige hlaford ??inne,
sunu Healfdenes, secean cwomon,
leodgebyrgean; wes ??u us larena god.
Habba?? we to ????m m??ran micel ??rende,
Deniga frean, ne sceal ????r dyrne sum
wesan, ????s ic wene. ??u wast gif hit is
swa we so??lice secgan hyrdon
????t mid Scyldingum scea??ona ic nat hwylc,
deogol d??dhata, deorcum nihtum
eawe?? ??urh egsan uncu??ne ni??,
hyn??u ond hrafyl. Ic ????s Hro??gar m??g
??urh rumne sefan r??d gel??ran,
hu he frod ond god feond oferswy??e??,
gyf him edwendan ??fre scolde
bealuwa bisigu, bot eft cuman,
ond ??a cearwylmas colran wur??a??;
o????e a sy????an earfo????rage,
??reanyd ??ola??, ??enden ????r wuna??
on heahstede husa selest.
Weard ma??elode, ????r on wicge s??t,
ombeht unforht: ??ghw????res sceal
scearp scyldwiga gescad witan,
worda ond worca, se ??e wel ??ence??.
Ic ????t gehyre, ????t ??is is hold weorod
frean Scyldinga. Gewita?? for?? beran
w??pen ond gew??du; ic eow wisige.
Swylce ic magu??egnas mine hate
wi?? feonda gehwone flotan eowerne,
niwtyrwydne nacan on sande
arum healdan, o??????t eft byre??
ofer lagustreamas leofne mannan
wudu wundenhals to Wedermearce,
godfremmendra swylcum gife??e bi??
????t ??one hilder??s hal gedige??.
Gewiton him ??a feran. Flota stille bad,
seomode on sale sidf????med scip,
on ancre f??st. Eoforlic scionon
ofer hleorberan gehroden golde,
fah ond fyrheard; ferhwearde heold
gu??mod grimmon. Guman onetton,
sigon ??tsomne, o??????t hy s??l timbred,
geatolic ond goldfah, ongyton mihton;
????t w??s forem??rost foldbuendum
receda under roderum, on ????m se rica bad;
lixte se leoma ofer landa fela.
Him ??a hildedeor hof modigra
torht get??hte, ????t hie him to mihton
gegnum gangan; gu??beorna sum
wicg gewende, word ??fter cw????:
M??l is me to feran; f??der alwalda
mid arstafum eowic gehealde
si??a gesunde. Ic to s?? wille
wi?? wra?? werod wearde healdan.
Str??t w??s stanfah, stig wisode
gumum ??tg??dere. Gu??byrne scan
heard hondlocen, hringiren scir
song in searwum, ??a hie to sele fur??um
in hyra gryregeatwum gangan cwomon.
Setton s??me??e side scyldas,
rondas regnhearde, wi?? ????s recedes weal,
bugon ??a to bence. Byrnan hringdon,
gu??searo gumena; garas stodon,
s??manna searo, samod ??tg??dere,
??scholt ufan gr??g; w??s se iren??reat
w??pnum gewur??ad. ??a ????r wlonc h??le??
oretmecgas ??fter ????elum fr??gn:
Hwanon ferigea?? ge f??tte scyldas,
gr??ge syrcan ond grimhelmas,
heresceafta heap? Ic eom Hro??gares
ar ond ombiht. Ne seah ic el??eodige
??us manige men modiglicran.
Wen ic ????t ge for wlenco, nalles for wr??csi??um,
ac for hige??rymmum Hro??gar sohton.
Him ??a ellenrof andswarode,
wlanc Wedera leod, word ??fter spr??c,
heard under helme: We synt Higelaces
beodgeneatas; Beowulf is min nama.
Wille ic asecgan sunu Healfdenes,
m??rum ??eodne, min ??rende,
aldre ??inum, gif he us geunnan wile
????t we hine swa godne gretan moton.
Wulfgar ma??elode ????t w??s Wendla leod;
w??s his modsefa manegum gecy??ed,
wig ond wisdom: Ic ????s wine Deniga,
frean Scildinga, frinan wille,
beaga bryttan, swa ??u bena eart,
??eoden m??rne, ymb ??inne si??,
ond ??e ??a ondsware ??dre gecy??an
??e me se goda agifan ??ence??.
Hwearf ??a hr??dlice ????r Hro??gar s??t
eald ond anhar mid his eorla gedriht;
eode ellenrof, ????t he for eaxlum gestod
Deniga frean; cu??e he dugu??e ??eaw.
Wulfgar ma??elode to his winedrihtne:
Her syndon geferede, feorran cumene
ofer geofenes begang Geata leode;
??one yldestan oretmecgas
Beowulf nemna??. Hy benan synt
????t hie, ??eoden min, wi?? ??e moton
wordum wrixlan. No ??u him wearne geteoh
??inra gegncwida, gl??dman Hro??gar.
Hy on wiggetawum wyr??e ??incea??
eorla ge??htlan; huru se aldor deah,
se ????m hea??orincum hider wisade.
Hro??gar ma??elode, helm Scyldinga:
Ic hine cu??e cnihtwesende.
W??s his ealdf??der Ecg??eo haten,
????m to ham forgeaf Hre??el Geata
angan dohtor; is his eafora nu
heard her cumen, sohte holdne wine.
??onne s??gdon ????t s??li??ende,
??a ??e gifsceattas Geata fyredon
??yder to ??ance, ????t he XXXtiges
manna m??gencr??ft on his mundgripe
hea??orof h??bbe. Hine halig god
for arstafum us onsende,
to Westdenum, ????s ic wen h??bbe,
wi?? Grendles gryre. Ic ????m godan sceal
for his mod??r??ce madmas beodan.
Beo ??u on ofeste, hat in gan
seon sibbegedriht samod ??tg??dere;
gesaga him eac wordum ????t hie sint wilcuman
Deniga leodum.
[] word inne abead:
Eow het secgan sigedrihten min,
aldor Eastdena, ????t he eower ????elu can,
ond ge him syndon ofer s??wylmas
heardhicgende hider wilcuman.
Nu ge moton gangan in eowrum gu??geatawum
under heregriman Hro??gar geseon;
l??ta?? hildebord her onbidan,
wudu, w??lsceaftas, worda ge??inges.
Aras ??a se rica, ymb hine rinc manig,
??ry??lic ??egna heap; sume ????r bidon,
hea??oreaf heoldon, swa him se hearda bebead.
Snyredon ??tsomne, ??a secg wisode,
under Heorotes hrof
heard under helme, ????t he on heo??e gestod.
Beowulf ma??elode on him byrne scan,
searonet seowed smi??es or??ancum:
W??s ??u, Hro??gar, hal. Ic eom Higelaces
m??g ond mago??egn; h??bbe ic m??r??a fela
ongunnen on geogo??e. Me wear?? Grendles ??ing
on minre e??eltyrf undyrne cu??;
secga?? s??li??end ????t ????s sele stande,
reced selesta, rinca gehwylcum
idel ond unnyt, si????an ??fenleoht
under heofenes hador beholen weor??e??.
??a me ????t gel??rdon leode mine
??a selestan, snotere ceorlas,
??eoden Hro??gar, ????t ic ??e sohte,
for??an hie m??genes cr??ft minne cu??on,
selfe ofersawon, ??a ic of searwum cwom,
fah from feondum. ????r ic fife geband,
y??de eotena cyn ond on y??um slog
niceras nihtes, nearo??earfe dreah,
wr??c Wedera ni?? wean ahsodon,
forgrand gramum, ond nu wi?? Grendel sceal,
wi?? ??am agl??can, ana gehegan
??ing wi?? ??yrse. Ic ??e nu ??a,
brego Beorhtdena, biddan wille,
eodor Scyldinga, anre bene,
????t ??u me ne forwyrne, wigendra hleo,
freowine folca, nu ic ??us feorran com,
????t ic mote ana ond minra eorla gedryht,
??es hearda heap, Heorot f??lsian.
H??bbe ic eac geahsod ????t se ??gl??ca
for his wonhydum w??pna ne recce??.
Ic ????t ??onne forhicge swa me Higelac sie,
min mondrihten, modes bli??e,
????t ic sweord bere o????e sidne scyld,
geolorand to gu??e, ac ic mid grape sceal
fon wi?? feonde ond ymb feorh sacan,
la?? wi?? la??um; ????r gelyfan sceal
dryhtnes dome se ??e hine dea?? nime??.
Wen ic ????t he wille, gif he wealdan mot,
in ????m gu??sele Geotena leode
etan unforhte, swa he oft dyde,
m??gen Hre??manna. Na ??u minne ??earft
hafalan hydan, ac he me habban wile
dreore fahne, gif mec dea?? nime??.
Byre?? blodig w??l, byrgean ??ence??,
ete?? angenga unmurnlice,
mearca?? morhopu; no ??u ymb mines ne ??earft
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
  ignore (Buffer.contents b)

let benchmark = Type.Benchmark {init;run}
