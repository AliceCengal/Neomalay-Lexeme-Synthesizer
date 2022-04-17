# NEO-MALAY LEXEME SYNTHESIZER

https://alicecengal.github.io/Neomalay-Lexeme-Synthesizer/

Athran Abdul Rahman  
Universiti Teknologi Malaysia  
September 2017

**Neo-malay** here means that the language to be used in this study 
is not exactly any extant Malay language variety, but rather is a 
constructed language inspired by the Malay language.

**Lexeme Synthesizer** is a procedure whereby lexemes are synthesized 
according to the phonological and phonotactical rules of a language.

**Goal** --- to synthesize new lexemes that visually and aurally 
resemble extant native Malay words.

**Significance** --- a lexeme synthesizer may be useful for building 
a *lorem ipsum* generator localized for the Malay language. 
It may also be useful for inventing new names for writing fictions.

**Methods**
1. Analyse the phonology of Malay, then derive a phonology for Neomalay
2. Analyse the phonotactic rule of Malay, then derive a phonotactic rule 
   for Neomalay
3. Write a computer program that uses the rules set out in Step 1 & 2 to 
   generate new lexemes in Neomalay

For the linguistics details, refer to the PDF paper.

The synthesizer is implemented in the file `synth.scala`. 
Please don't tell me about Prolog.

**Sample Output**

raskeng   | ngumem    | ngoyos    | nyesper   | ngangkah  | rocop     | rongkot   | iscam
:---: | :---: | :---: | :---: | :---: | :---: | :---: | :---:
wecel     | menjam    | langgu    | lospol    | mabor     | kosong    | hances    | muku
nguyau    | kescet    | rimbas    | teskau    | mescau    | teskek    | dapi      | nyewes
descan    | ecoi      | kidom     | senger    | reteh     | jimak     | jisku     | monggoh
raget     | huscu     | lujel     | anyom     | lemes     | hutek     | jegu      | recoh
musci     | nyejong   | betem     | wantap    | loscok    | nagan     | rapos     | temen
ngangah   | sesti     | hicor     | isoi      | jinyak    | askas     | mesop     | penggel
serem     | mecep     | hingsar   | uspang    | pesel     | deceng    | udep      | ebol
mespel    | pangses   | lestel    | gestep    | nunges    | cegan     | becoi     | gemem
buhas     | nyungau   | tispan    | nyagang   | dohor     | watong    | heskos    | beler
buncang   | lesi      | mendep    | mahek     | bagi      | pinggang  | guban     | kescang

