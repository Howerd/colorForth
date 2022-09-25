\ .\cf2022\cf2022Ref.img converted by colorForthScan V1.0 2022 Apr 08
\ File MD5 = 3DB29AF8FF279E6C2980C6E64C290014  sparsely-skated     

\ MagentaV  is the colorForth Magenta Variable 
: MagentaV ( initial -- )   create , ; \ Runtime: ( -- a ) 

\ Block 64 
( colorforth cf2022 2022 Apr 08 ) 
( processor clock ) #0 MagentaV mhz 
( dump ) #5952 MagentaV x #0 MagentaV y ( ld ) #64 MagentaV lblk 
#2 #12 +thru
: dump #78 load ;
: icons #80 ld ;        : serve #506 ld ;
: north #92 ld ;        : rtc #96 ld ;
: lan #98 ld ;          : colors #102 ld ;
: wood #106 ld ;        : mand #108 ld ;
: sound #114 ld ;       : gr #118 ld ;
: eth #176 ld ;         : life #272 ld ;
: ed #252 ld ;          : slime #246 ld ;
: int #288 ld ;         : xx #278 load ;
: info ver dump ;       : staks #504 ld ; 

( hardware ) #0 MagentaV rng
: chm ( -- ) #0 mhz ! $1740 x ! #0 y ! #64 lblk ! $00010000
: ch ( n-- ) #64 block swap md5 dump ;
: hlp randq rng ! logo pause calkhz 
   onesec @ #1000 / mhz ! e ; 
mark empty hlp 

( Press the * key to see the comment block ) 

( Press F1 ) 

\ Block 65 
\ ( Based on colorforth 2001 Jul 31 by Chuck Moore ) 
\ ( released into the Public Domain. ) 
\ ( This block is loaded at power up. Press F1 for help )
\ : dump ( instant compile version of DUMP )
\ : icons ( edit the character font icons )
\ : north ( North Bridge PCI chip display )
\ : rtc ( Real Time Clock display )
\ : colors ( 3-axis rgb colour display )
\ : wood ( imitation pine blockboard )
\ : mand ( display the Mandeldrot set )
\ : sound ( control the PC speaker )
\ : gr ( graphics - type ok to run the demo )
\ : life ( Conways game of life )
\ : ed ( the editor partly converted to colorforth )
\ : slime ( watch out for the slugs! )
\ : int ( 1000 Hz timer interrupt )
\ : xx ( colorforth explorer )
\ : ch ( show MD5 of n bytes starting at block 64 )
\ : chm ( show MD5 of system blocks )
\ : help ( press the space bar to leave the editor, then type the keys indicated in the keypad in the bottom right of the 
\ screen, then the space bar to execute the word. Type ) e ( or ) #64 edit ( to run the editor. ) 
\ info ( to view the boot system version ) 
\ seeb ( to toggle display of blue words ) 
\ hlp ( shows help and clock speed ) 

\ Block 66 
macro
: ?f $C021 2, ;
: 0if $75 2, here ;
: +if $78 2, here ;
: 1+ ( n-n ) $40 1, ;
: 1- ( n-n ) $48 1, ;
: 2/ ( n-n ) $F8D1 2, ;
: time ( -u ) qdup $310F 2, ;
: shl ( uc-u ) ?lit $E0C1 2, 1, ;
: shr ( uc-u ) ?lit $E8C1 2, 1, ;
: r@ qdup $8B 1, $C7 1, ;
: sti $FB 1, ; ( enable interrupts )
: cli $FA 1, ; ( disable interrupts ) forth
: cli cli ;
: sti sti ;
: nul ;
: time time ; 

\ Block 67 
\ ( Pentium macros: )
\ : ?f ( set flags to reflect tos )
\ : 0if ( if zero ... then jnz aids in clarity )
\ : +if ( js, this complements the set )
\ : 1- ( subtract 1 )
\ : 2/ ( divide by 2 )
\ : qdup ( is the new name for ?dup )
\ : time ( return Pentium instruction counter )
\ : lshift ( shift u left c places )
\ : rshift ( shift u right c places )
\ : r@ ( copies the top of the return stack to TOS )
\ : sti ( enable device interrupts )
\ : cli ( disable them )
\ : a, 

\ Block 68 
( more macros ) macro
: swap $168B 2, $C28B0689 , ;
: 0 qdup $C031 2, ;     : if $74 2, here ;
: -if $79 2, here ;     : a qdup $C28B 2, ;
: a! ?lit if $BA 1, , ; then $D08B 2, drop ;
: 1@ $8A 2, ;           : 1! a! $0288 2, drop ;
: p@ ( a-n ) qdup a! $EC 1, ;
: p! ( na- ) a! $EE 1, drop ;
: 2* $E0D1 2, ;
: a, , ;
: @ ?lit if qdup $058B 2, , ; then $8B 2, 0 , ;
: ! ?lit if ?lit if $05C7 2, swap , , ; then $0589 2, , drop ; then a! $0289 2, 0 , drop ;
: nip $0004768D 3, ;
: + ?lit if $05 1, , ; then $0603 2, nip ;
: xor $0633
: binary ?lit if swap #2 + 1, , ; then 2, nip ;
: and $0623 binary ;
: or $060B binary ;
: u+ ?lit if $0681 2, , ; then $00044601 3, drop ;
: ? ?lit $A9 1, , ; 

\ Block 69 
\ ( Pentium macros: 1, 2, 3, , compile 1-4 bytes )
\ : drop ( lodsd, flags unchanged, why sp is in ESI )
\ : over ( sp 4 + @ )
\ : swap ( sp xchg )
\ : 0 ( 0 0 xor, macro 0 identical to number 0 )
\ : a ( 2 0 mov, never used? )
\ : a! ( 0 2 mov, unoptimized )
\ : 1@ ( fetch byte from byte address )
\ : 1! ( store byte to byte address )
\ : p@ p-n ( fetch byte from port )
\ : p! np ( store byte to port )
\ : @ ( EAX 4 *, unoptimized )
\ : ! ( EDX 4 * )
\ : nop ( used to thwart look-back optimization )
\ : - ( ones-complement )
\ : 2*
\ : 2/
\ : if ( jz, flags set, max 127 bytes, leave address )
\ : -if ( jns, same )
\ : then ( fix address - in kernel )
\ : push ( EAX push )
\ : pop ( EAX pop )
\ : u+ ( add to 2nd number, literal or value )
\ : ? ( test bits, set flags, literal only! ) 

\ Block 70 
( even more macros )
: over qdup $0004468B 3, ;
: push $50 1, drop ;
: pop qdup $58 1, ;
: invert ( n-n ) $D0F7 2, ;
: for push begin ;
: *next swap
: next $75240CFF
: 0next , here invert + 1, $0004C483 3, ;
: -next $79240CFF 0next ;
: i qdup $0024048B 3, ;
: *end swap
: end $EB 1, here invert + 1, ;
: +! ?lit if ?lit if $0581 2, swap , , ; then $0501 2, , drop ; then a! $0201 2, drop ;
: nop $90 1, ;
: align here invert #3 and drop if nop align ; then ;
: or! a! $00950409 3, 0 , drop ;
: * $0006AF0F 3, nip ;
: */ $C88B 2, drop $F9F72EF7 , nip ;
: /mod swap $99 1, $16893EF7 , ;
: / /mod nip ;
: mod /mod drop ; 

\ Block 71 
\
\ : - n-n ( ones complement negate , xor )
\ : for n ( push count onto return stack, falls into ) begin
\ : begin -a ( current code address - byte )
\ : *next aa-aa ( swap ) for ( and ) if ( addresses )
\ : next a ( decrement count, jnz to ) for, ( pop return stack when done )
\ : -next a ( same, jns - loop includes 0 )
\ : i -n ( copy loop index to data stack )
\ : end a ( jmp to ) begin
\ : +! na ( add to memory, 2 literals optimized )
\ : align ( next call to end on word boundary )
\ : or! na ( inclusive-or to memory, unoptimized )
\ : * mm-p ( 32-bit product )
\ : */ mnd-q ( 64-bit product, then quotient )
\ : /mod nd-rq ( remainder and quotient )
\ : / nd-q ( quotient )
\ : mod nd-r ( remainder )
\ : time -n ( Pentium cycle counter, calibrate to get actual clock rate ) 

\ Block 72 
( Compiled macros ) forth
: r@ ( -n ) r@ ;
: @ ( a-n ) @ ;
: ! ( an- ) ! ;
: + ( nn-n ) + ;
: 1+ ( u--u ) 1+ ;
: 1- ( u--u ) 1- ;
: invert ( n-n ) invert ;
: */ ( nnn-n ) */ ;
: * ( nn-n ) * ;
: / ( nn-n ) / ;
: 2* ( n-n ) 2* ;
: 2/ ( n-n ) 2/ ;
: dup ( n-nn ) dup ;
: swap ( nn-nn ) swap ;
: over over ; 
( Arithmetic )
: negate ( n-n ) invert #1 + ;
: - ( nn-n ) negate + ;
: min ( nn-n ) less if drop ; then swap drop ;
: abs ( n-u ) dup negate
: max ( nn-n ) less if swap then drop ;
: v+ ( vv-v ) push u+ pop + ;
: save sss ;
: sa sss e ; 

\ Block 73 
\ ( These macros may be ) yellow, ( others may not )
\ : block n-a ( block number to word address )
\ : r@ ( copies the top of the return stack to stack )
\ : @ etc ( Arithmetic )
\ : negate n-n ( when you just cant use ) -
\ : min nn-n ( minimum )
\ : abs n-u ( absolute value )
\ : max nn-n ( maximum )
\ : v+ vv-v ( add 2-vectors )
\ : save ( write colorforth to a bootable USB drive )
\ : sa ( save, then show edit screen ) 

\ Block 74 
( Relative load blocks )
: ll ( -- ) blk @ load ;
: sect ( --asn ) blk @ block blk @ 2* #2 ;
: ss ( -- ) sect writes drop drop ;
: uu ( -- ) sect reads drop drop ; 
: ld ( n- ) dup lblk ! load ;
: vv ( -- ) lblk @ edit ;
: help ( -- ) lblk @ #1 + edit ; 
( Real Time Clock )
: rtc@ ( t-c ) $70 p! $71 p@ ;
: rtc! ( ct- ) $70 p! $71 p! ;
: hi ( -- ) #10 rtc@ $80 and drop 0if hi ; then ;
: lo ( -- ) #10 rtc@ $80 and drop if lo ; then ; 
: calkhz ( -- ) hi lo counter hi lo counter swap - 
   dup onesec ! #1 rshift #250 + #500 / dup khz ! ;
: ms ( n- ) khz @ * counter + begin pause dup counter 
   invert + drop -if drop ; then end drop ;
: secs ( n- ) for pause lo hi next ; macro
: swapb ( w-w ) $E086 2, ; forth
: split ( w--cc ) dup swapb $FF and swap $FF and ; 

\ Block 75 
\
\ : nload ( loads the next source block : b+2 )
\ : +load ( loads the source block : b+n )
\ : blk ( where the current blk happens to be kept )
\ : ll ( load the current edit blk )
\ : ss ( save the sector containing the current edit block to the floppy disc )
\ : lblk ( holds the last block loaded by )
\ : ld
\ : vv ( edits the last block loaded by ld )
\ : rtc@ reg-n ( fetch reg from rtc )
\ : rtc! n reg- ( store in rtc register )
\ : hi ( wait till Update In Progress bit is high )
\ : lo ( wait till UIP bit is low )
\ : calkhz ( calibrate the processor clock using the RTC )
\ : ms ( wait for n milliseconds )
\ : secs ( wait for n seconds )
\ : swapb ( swap the two low bytes )
\ : split ( split the low two bytes )
\ : vframe ( byte address of the video frame buffer ) 

\ Block 76 
( Colors etc )
: white $00FFFFFF rgb color ; : red $00FF0000 rgb color ;
: green $FF00 rgb color ; : blue $FF rgb color ;
: silver $00BFBFBF rgb color ; : yellow $FFE0 color ;
: orange $00E04000 rgb color ; : black $00 rgb color ;
: 5* #5 for 2emit next ;
: cf #25 dup at red $72 $6F $6C $6F $63 5* green $68 $74 $72 $6F $46 5* ;
: logo show black screen #800 #710 blue box #600 #50 at #1024 #620 red box #200 #100 at #700 #500 green box text cf keypa d ;
: noshow show keypad ;
: lshift ( uc-u ) $1F and ?f 0if drop ; then for #1 shl next ;
: rshift ( uc-u ) $1F and ?f 0if drop ; then for #1 shr next ;
: rand32 ( -n ) time dup #16 lshift xor ;
: string pop ;
: 1@ ( a-c ) 1@ $0F and ;
: 1! ( ac- ) 1! ; 

\ Block 77 
\
\ : colors ( specified as rgb: 888 )
\ : screen ( fills screen with current color )
\ : at xy ( set current screen position )
\ : box xy ( lower-right of colored rectangle )
\ : 5* ( displays 5 large characters )
\ : cf ( displays ) colorforth
\ : logo ( displays colorforth logo )
\ : empty ( also displays the logo )
\ : lshift ( shift u left c places )
\ : rshift ( shift u right c places )
\ : show ( background task executes following code repeatedly )
\ : keyboard ( displays keypad and stack )
\ : string ( returns the address of the string following )
\ : rand32 ( returns a 3 bit random number ) 

\ Block 78 
( Dump names )
: .cell ( a-a ) orange dup @ #4 for dup $FF and emit $0100 / next drop white ;
: one dup dup @ dup push h. space dup h. pop space swap .cell drop space space space space dup dotsf drop white cr ;
: lines for one #4 + next drop ;
: dump ( a- ) $0FFFFFFC and x !
: r show black screen x @ #16 text lines cr x @ #16 for .cell #4 + next drop keypad ;
: it @ + @ dup h. space ;
: lines for white i x it i y it xor drop if red then i . cr -next ;
: cmp show blue screen text #19 lines red x @ h. space y @ h. keypad ;
: u $40
: +xy dup x +! y +! ;
: d $FFFFFFC0 +xy ;
: ati $F4100000 ( ff7fc000 ) xor
: byte #4 / dump ;
: fix for #0 over ! #1 + next ; dump 

\ Block 79 
\ ( Does not say empty, compiles on top of application )
\ : x -a ( current address )
\ : one a-a ( line of display )
\ : lines an
\ : dump a ( background task continually displays memory : decodes the value as a name and ASCII )
\ : u ( increment address )
\ : d ( decrement )
\ : ati ( address of AGP graphic registers )
\ : byte a ( byte address dump )
\ : fix an-a ( test word )
\ : ver ( show the kernel version information )
\ : cmp ( shows data at both ) x ( and ) y ( addresses ) 

\ Block 80 
( App: Icons font editor ) empty 
( icon number ) #52 MagentaV ic ( cursor ) #0 MagentaV cu 
macro : @w $8B66 3, ; : !w a! $00028966 3, drop ;
: *byte $C486 2, ; forth 
: sq xy @ $00010000 /mod #16 + swap #16 + box #17 #0 +at ;
: loc ic @ $FF and
: tofont ( n--a ) #16 #24 #8 */ * font @ + ;
: 0/1 $8000 ? if green sq ; then blue sq ;
: row dup @w *byte #16 for 0/1 2* next drop #-17 #16 * #17 +at ;  : cpl #32 ;
: showall ( -- ) #2 lm iconw cpl * rm ic @ cpl /mod iconh * #448 #2 - + swap iconw * swap over over at red #16 #4 + u+ #24 
+ #4 + box white 
   #0 #2 #448 at #256 for dup emit #1 + next drop ;
: ikon loc #24 for row #2 + next drop ;
: adj #17 * swap ;
: cursor cu @ #16 /mod adj adj over over at red #52 u+ #52 + box ;
: ok show page cursor #18 dup at ikon text blue #400 #400 at    ef #416 #424 box #400 #400 at white    ef ic @ dup emit space 
dup green . $30 emit $78 emit #2 h.n showall keypad ; 
: fcopy tofont swap tofont swap #16 #24 #8 */ cmove ; 
nload ok h 

\ Block 81 
\ ( Draw big-bits icon )
\ : @w a-n ( fetch 16-bit word from byte address )
\ : !w na ( store same )
\ : *byte n-n ( swap bytes )
\ : ic -a ( current icon )
\ : cu -a ( cursor )
\ : sq ( draw small square )
\ : xy -a ( current screen position, set by ) at
\ : loc -a ( location of current icons bit-map )
\ : 0/1 n-n ( color square depending on bit 15 )
\ : row a-a ( draw row of icon )
\ : +at nn ( relative change to screen position )
\ : ikon ( draw big-bits icon )
\ : adj nn-nn ( magnify cursor position )
\ : cursor ( draw red box for cursor )
\ : ok ( background task to continually draw icon, icon number at bottom ) 

\ Block 82 
( Edit icon ) 
: icmv ( n-- ) ic @ + $FF and ic ! ;
: +ic #1 icmv ;         : -ic #-1 icmv ;
: ++ic cpl icmv ;       : --ic cpl negate icmv ;
: bit cu @ 2/ 2/ 2/ 2/ 2* loc + $00010000 cu @ $0F and #1 + for 2/ next *byte ;
: toggle bit over @w xor swap !w ; 

: td toggle             : d #16
: wrap cu @ + #16 #24 * dup u+ /mod drop cu ! ;
: tu toggle             : u #-16 wrap ;
: tr toggle             : r #1 wrap ;
: tl toggle             : l #-1 wrap ;
: nul ;
: h keypd 
nul nul quit nul        tl tu td tr 
l u d r                 -ic --ic ++ic +ic 
nul nul nul nul         nul nul nul toggle 
   nul nul nul nul 
$2500 , $13121110 dup , , $2B16152D , #0 , $80000000 , #0 , 

\ Block 83 
\ ( Edit icon )
\ : t ( toggles the current pixel )
\ : ludr ( left up down right )
\ : . ( top row toggles and moves )
\ : -+ ( select icon to edit ) 

\ Block 84 
( Print PNG to disk ) #1024 MagentaV w #768 MagentaV h #1 MagentaV d 
#6 +load #4 +load #2 +load
: -crc ( a ) here over negate + crc . ;
: crc -crc ;
: wd ( -a ) here #3 and drop if #0 1, wd ; then here #2 2/s ;
: bys ( n-a ) . here swap , ;
: plte $45544C50 #48 bys $00 3, $00FF0000 3, $FF00 3, $00FFFF00 3, $FF 3, $00FF00FF 3, $FFFF 3, $00FFFFFF 3, $00 3, $00C00000 
3, $C000 3, $00C0C000 3, $C0 3, $00C000C0 3, $C0C0 3, $00C0C0C0 3, crc ;
: png ( awh ) d @ / h ! d @ / w ! wd swap $474E5089 , $0A1A0A0D , ( ihdr ) $52444849 #13 bys w @ . h @ . $0304 , $00 1, crc 
plte ( idat ) $54414449 #0 bys swap deflate crc ( iend ) $444E4549 #0 bys crc wd over negate + ;
: at #1024 * + 2* vframe + ;
: full #4 d ! #0 dup at #1024 #768 png ;
: pad #1 d ! #46 #-9 + #22 * nop #25 #-4 + #30 * at #9 #22 * nop #4 #30 * png ;
: go #1 d ! #1024 w ! #768 h ! #0 #0 at #1024 #768 png raw ; go e 

\ Block 85 
\ ( Print PNG to disk )
\ : frame ( the video frame buffer )
\ : -crc ( a )
\ : crc
\ : wd ( -a )
\ : bys ( n-a )
\ : plte
\ : png ( awh )
\ : at
\ : full
\ : pad
\ : go ( copy the screen image as a PNG file to the floppy disk block 270 and up. ) 

\ Block 86 
( lz77 ) macro
: @w $8B66 3, ;
: *byte $C486 2, ;
: !b a! $0289 2, drop ; forth
: *bys dup #16 2/s *byte swap $FFFF and *byte $00010000 * + ;
: . *bys , ;
: +or over invert and or ;
: 0/1 $10 ? if $1E and $1E or drop if #7 ; then $0F ; then #0 and ;
: 4b dup 0/1 #9 and over #6 2/s 0/1 $0A and +or swap #11 2/s 0/1 $0C and +or $08 or ;
: pix dup @w d @ 2* u+ 4b ;
: row 1, dup w @ 2/ dup #1 + dup 2, invert 2, #0 dup 1, +adl for pix #16 * push pix pop or dup 1, +adl next drop +mod d @ 
#1024 #2 * * + ;
: deflate $0178 2, #1 #0 adl! h @ #-1 + for #0 row next #1 row drop ad2 @ *byte 2, ad1 @ *byte 2, here over #4 + negate + 
*bys over #-4 + !b ; 

\ Block 88 
( Crc ) macro
: 2/s ?lit $E8C1 2, 1, ;
: 1@ $8A 2, ; forth #36054 MagentaV ad1 #54347 MagentaV ad2
: array ( -a ) pop #2 2/s ;
: bit ( n-n ) #1 ? if #1 2/s $EDB88320 or ; then #1 2/s ;
: fill ( nn ) for dup #8 for bit next , #1 + next drop ;
: table ( -a ) align array #0 #256 fill
: crc ( an-n ) #-1 swap for over 1@ over or $FF and table + @ swap #8 2/s or #1 u+ next invert nip ;
: +adl ( n ) $FF and ad1 @ + dup ad2 @ +
: adl! ad2 ! ad1 ! ;
: +mod ad1 @ #65521 mod ad2 @ #65521 mod adl! ; 

\ Block 90 
( DOS file )
: blks #256 * ;
: w/c #18 blks ;
: buffer block ;
: size ( -a ) buffer #0 #1 reads buffer $098F + ;
: set ( n ) ! buffer     s #1 writes ;
: cyls ( n-nn ) #1 swap w/c #-1 + + w/c / ;
: put ( an ) dup 2* 2* size set cyls writes /flop ;
: raw ( an- ) #15 swap 2* 2* w/c #-1 + + w/c / writes /flop ;
: get ( a ) size @ #3 + 2/ 2/ cyls reads /flop ;
: .com #0 #63 blocks put ; 

\ Block 91 
\
\ : blks n-n ( size in blocks to words )
\ : w/c -n ( words per cylinder )
\ : buffer -a ( 1 cylinder required for floppy dma )
\ : size -a ( locate size of 2nd file. Floppy has first FILLER then FILE allocated. FILLER is 2048 bytes, to fill out cylind 
\er 0. Names at most 8 letters, all caps. Directory starts at ) buffer $0980 +
\ : set n ( size. FILE must be larger than your file. )
\ : cyls n-nn ( starting cylinder 1 and number of cylinders )
\ : raw an ( write raw data to cyl 15 , block 270 )
\ : put an ( write file from address )
\ : get a ( read file to address ) 

\ Block 92 
( App: North Bridge ) empty macro
: 4@ dup $ED 1, ;
: 4! $EF 1, drop ; forth #2048 MagentaV dev
: nb $00 dev ! ;
: sb $3800 dev ! ;
: agp $0800 dev ! ;
: ess $6800 dev ! ;
: ric $7800 dev ! ;
: win $8000 dev ! ;
: ati $00010000 dev ! ;
: add $0CF8 a! 4! $0CFC a! ;
: q $80000000 + add 4@ ;
: en $8004 q #-4 and xor 4! ;
: dv dup $0800 * q swap #1 + ;
: regs dev @ #19 #4 * + #20 for dup q h. space dup h. cr #-4 + next drop ;
: devs #0 #33 for dup q dup #1 + drop if dup h. space drop dup #8 + q dup h. space over h. cr then drop $0800 + next drop 
;
: ok show black screen text regs keypad ;
: ko show black screen text devs keypad ;
: u $40 dev +! ;
: d #-64 dev +! ;
: test $FF00 + a! 4@ ; ok 

\ Block 93 
\ ( Display the PCI interface chip registers ) 

\ Block 94 
( ASCII )
: cf-ii string ( 0*00 ) $6F747200 , $696E6165 , $79636D73 , $7766676C , ( 0*10 ) $62707664 , $71757868 , $33323130 , $37363534 
, ( 0*20 ) $2D6A3938 , $2F7A2E6B , $2B213A3B , $3F2C2A40 , ( 0*30 ) $4F545200 ,
: ch $FFFFFFF0 and unpack cf-ii + 1@ $FF and ;
: ii-cf string ( 0x20 ) $64632A00 , $7271706F , $2B2D6E6D , $2725232E , ( 0x30 3210 ) $1B1A1918 , ( 7654 ) $1F1E1D1C , ( ..98 
) $28292120 , $2F6C6B6A , ( 0x40 CBA@ ) $3A43352C , ( GFED ) $3D3E3440 , ( KJIH ) $54523744 , ( ONML ) $3336393C , ( 0x50 
SRQP ) $38314742 , ( WVUT ) $3F414632 , ( .ZYX ) $58563B45 , $75745973 , ( 0x60 cba. ) $0A130576 , ( gfed ) $0D0E0410 , ( kjih 
) $24220714 , ( onml ) $0306090C , ( 0x70 srqp ) $08011712 , ( wvut ) $0F111602 , ( .zyx ) $77260B15 , $62617879 ,
: chc $FFFFFFE0 + ii-cf + 1@ $FF and ;
: tst #2000 block dup #4 * #-1 + $60 for $01 + $80 i negate + over 1! next drop dump ; #51 MagentaV qch
: rr ( c-c ) qch ! $20 $60 for $01 + dup chc qch @ negate + drop 0if pop drop ; then next $7F and ; 

\ Block 95 
\ ( Convert colorforth chars to and from ASCII )
\ : cf-ii ( conversion table )
\ : ch ( convert colorforth character to ASCII )
\ : ii-cf ( conversion table )
\ : chc ( convert ASCII to colorforth )
\ : tst ( create a table of ASCII characters )
\ : r ( scan the ii-cf table to perform cf-ii . Used to cross-reference the two tables )
\ : info ( display the ASCII version information in the last 64 bytes of block 11 . Type u to see more . ) 
\    ( dump takes a byte address ) 

\ Block 96 
( App: RTC Real Time Clock ) empty 
: bcd ( -c ) rtc@ #16 /mod #10 * + ;
: hms ( -n ) lo #4 bcd #100 * #2 bcd + #100 * #0 bcd + ;     s
: ymd ( -n ) lo #9 bcd #2000 + #100 * #8 bcd + #100 * #7 bcd + ;
: day ( -c ) lo #6 bcd ;
: crlf ( Port Dump )
: one ( n-n ) space yellow dup rtc@ h.2 blue space dup . cr ;
: lines ( sn- ) for one #-1 + next drop ;
: ok show page text cr #15 #16 lines white cr ymd . 
   hms . day . keypad ;
: h 
keypd nul nul quit      nul nul nul nul 
nul nul nul nul         nul nul nul nul 
nul nul nul nul         nul nul nul nul 
   nul nul nul nul nul 
$00250000 , #0 , #0 , #0 , #0 , #0 , #0 , 
ok 

\ Block 97 
\ ( RTC Real Time Clock )
\ : . ( displays the PC clock registers )
\ : bcd bcd-n ( bcd to binary )
\ : hms -n ( hours+mins+secs )
\ : ymd -n ( year+month+day )
\ : day -n ( day of the week )
\ : rtc ( display the Real Time Clock registers )
\ : one ( display one line )
\ : lines ( display n lines starting at s )
\ : ok ( display task ) 

\ Block 98 
( LAN ) empty $03F8 nload init
: no block #4 * #1024 ;
: send no for dup 1@ xmit #1 + next drop ;
: receive no for rcv over 1! #1 + next drop ;
: no #18 #7 #18 * ;
: backup no for dup send #1 + next drop ;
: accept no for dup receive #1 + next drop ; 

\ Block 99 
\ 

\ Block 100 
( Serial 3f8 2e8 1050 ) macro
: 1@ $8A 2, ;
: 1! a! $0288 2, drop ; forth
: r #0 + + ;
: 9600 #12 ;
: 38400 #3 ;
: 115200 #1 ;
: b/s $83 #3 r p! 38400 #0 r p! #0 #1 r p! #3 #3 r p! ;
: init b/s ( 16550 ) #1 #2 r p! #0 #4 r p! ;
: xmit ( n ) #5 r p@ $20 and drop if #0 r p! ; then pause xmit ;
: cts #6 r p@ $30 and $30 xor drop if cts ; then xmit ;
: st #6 r p@
: xbits $30 and $10 / dup #1 and 2* 2* + 2/ ;
: st! #4 r p! ;
: ?rcv #5 r p@ #1 and drop if #0 r p@ then ;
: rcv ?rcv if ; then pause rcv ; lblk @ edit 

\ Block 101 
\
\ : 1@ a-n ( fetch byte from byte address )
\ : 1! na ( store byte to byte address )
\ : r n-p ( convert relative to absolute port address. Base port on stack at compile time. Compiled as literal at yellow 
\-green transition )
\ : 9600
\ : 115200 ( baud-rate divisors. These are names, not numbers )
\ : b/s ( set baud rate. Edit to change )
\ : init ( initialize uart )
\ : xmit n ( wait for ready and transmit byte )
\ : cts n ( wait for clear-to-send then xmit )
\ : st -n ( fetch status byte )
\ : xbits n-n ( exchange status bits )
\ : st! n ( store control byte )
\ : ?rcv ( fetch byte if ready. Set flag to be tested by ) if
\ : rcv -n ( wait for ready and fetch byte ) 

\ Block 102 
( App: Colors ) empty 
#4210752 MagentaV col #4210752 MagentaV del
: lin dup 2/ 2/ dup 2* line ;
: hex xy @ #7 and over 2/ for lin #7 + next over for lin next swap 2/ for #-7 + lin next drop ;
: +del del @ nop
: petal and col @ + $00F8F8F8 and rgb color #100 hex ;
: -del del @ $00F8F8F8 xor $00080808 + ;
: rose #0 +del #-176 #-200 +at $00F80000 -del petal #352 #-200 +at $00F80000 +del #-264 #-349 +at $F800 -del petal #176 #-200 
+at $F8 +del #-176 #98 +at $F8 -del petal #176 #-200 +at $F800 +del ;
: ok show page #512 #282 at rose text col @ h. space del @ $FF and h. keypad ; nload ok h e 

\ Block 103 
\ ( Draws 7 hexagons. Colors differ along red, green and blue axes. )
\ : col ( color of center hexagon )
\ : del ( color difference )
\ : lin n ( draws 1 horizontal line of a hexagon )
\ : hex n ( draws top, center and bottom. Slope 7 x to 4 y is 1.750 compared to 1.732 )
\ : +del n ( increment color )
\ : -del n
\ : petal n ( draw colored hexagon )
\ : rose ( draw 7 hexagons )
\ : ok ( describe screen. Center color at top ) 

\ Block 104 
( Colors keypad )
: in del @ 2* $00404040 min del ! ;
: out del @ 2/ $00080808 max del ! ;
: r $00F80000
: +del del @
: +col and col @ + $00F8F8F8 and col ! ;
: g $F800 +del ;
: b $F8 +del ;
: -r $00F80000 -del +col ;
: -g $F800 -del +col ;
: -b $F8 -del +col ;
: nul ;
: h keypd nul nul quit nul -r -g -b nul r g b nul out nul nul in nul nul nul nul nul nul nul nul nul nul nul nul $00250000 
, $00626772 dup , , $2B00002D , #0 , #0 , #0 , 

\ Block 105 
\
\ : in ( increment color difference )
\ : out ( decrement it )
\ : r
\ : g
\ : b ( increment center color )
\ : -r
\ : -g
\ : -b ( decrement it )
\ : +del ( redefine with ; )
\ : +col ( change center color )
\ : nul ( ignore )
\ : h ( describe keypad ) 

\ Block 106 
( App: Wood ) empty #125810090 MagentaV x #-1123891786 MagentaV y 
#8286477 MagentaV inc #33554432 MagentaV frame #39 MagentaV dep #65056 MagentaV hole
: h0 #400000 inc ! #15 dep !
: home inc @ scrnw #2 / * negate x     s ! inc @ scrnh #2 / * y ! ; macro
: f* $2EF7 2, #26 shr $E2C1 2, #6 1, $C20B 2, nip ;
: w! a! $00028966 3, drop ; forth
: wf+ frame @ w! #2 frame +! ;
: om negate $FF + ;     : o5 om $03 shr $07E0 xor ;
: o4 $FC and #3 shl $1F xor ;
: o3 om $F8 and #8 shl $1F xor ;
: o2 #3 shr $F800 xor ;
: o1 om $FC and #3 shl $F800 xor ;
: o0 $F8 and #8 shl $07E0 xor ;
: order jump o0 o1 o2 o3 o4 o5 o0
: hue #8 shl #26 / dup $FF and swap #8 shr order ;
: vlen dup f* swap dup f* + ;
: vdup over over ;
: vndp push push vdup pop pop ;
: itr over dup f* over dup f* negate + push f* 2* pop swap v+ over 2* + 2/ vndp + + ;
: data ; #4 +load ok draw h 

\ Block 107 
\ ( Display an imitation pine blockboard screen ) 
\ 
\ ( This is based on a skewed Mandelbrot set with ) 
\ ( modified colors ) 

\ Block 108 
( App: Mandelbrot Set ) empty 
#-204800000 MagentaV x #153600000 MagentaV y #400000 MagentaV inc 
#34 MagentaV dep #33554432 MagentaV frame #0 MagentaV hole
: h0 #400000 inc ! #34 dep !
: home inc @ scrnw #2 / * negate x     s ! inc @ scrnh #2 / * y ! ; macro
: f* $2EF7 2, #26 shr $E2C1 2, #6 1, $C20B 2, nip ;
: w! a! $00028966 3, drop ; forth
: wf+ frame @ w! #2 frame +! ;
: hue ( n-n ) #8191 * ; dup dup + dup dup + + + dup dup + dup dup    ef + + ; #3142 * ; @ ;    ef
: vlen dup f* swap dup f* + ;
: vdup over over ;
: vndp push push vdup pop pop ;
: itr over dup f* over dup f* negate + push f* 2* pop swap v+ ;
: x: ( c- ) emit $3D emit ;
: data text #0 #0 at $78 x: x @ . $79 x: y @ . $69 x: inc @ . $64 x: dep @ . ; nload ok draw h 

\ Block 109 
\ 

\ Block 110 
( Mandelbrot Set )
: o 0 0 dep @ #1 max for vndp itr vdup vlen $F0000000 + drop -if *next drop drop hole @ ; then drop drop pop hue ;
: mh x @ swap scrnw for o wf+ inc @ u+ next nip ;
: mv y @ scrnh for mh inc @ negate + next drop ;
: +d #2 dep +! : -d #-1 dep +! dep @ #1 max dep !
: draw vframe frame ! mv data ;
: ok c show keypad ;
: l inc @ scrnw #1 - #8 */ negate x +! draw ;
: u inc @ scrnh #1 - #8 */ y +! draw ;
: d inc @ scrnh #1 - #8 */ negate y +! draw ;
: r inc @ scrnw #1 - #8 */ x +! draw ;
: +z inc @ #3 max dup scrnw #1 - #8 */ x +! dup scrnh #1 - #8 */ negate y +! #3 #4 */ #3 max inc ! draw ;
: -z inc @ #10000000 min dup scrnw #1 - #8 */ negate x +! dup scrnh #1 - #8 */ y +! #4 #3 */ inc ! draw ;
: hh home draw ; : hh2 h0 draw ;
: h keypd nul nul quit nul -d nul nul +d l u d r -z hh hh2 +z nul nul nul nul nul nul nul nul nul nul nul nul $2500 , $2B00002D 
, $13121110 , $2B30482D , #0 , #0 , #0 , 

\ Block 111 
\ ( More Mandelbrot ) 
\ ( ludr move the cursor left right up down ) 
\ ( - + top row change depth detail ) 
\ ( - + bottom row change zoom ) 
\ ( h centres the image to the home location ) 
\ ( 0 resets depth and zoom ) 

\ Block 112 


\ Block 114 
( App: Sounds make a noise ) empty 
#25 MagentaV tempo #0 MagentaV mute #2259 MagentaV period
: tn ( ft- ) tempo @ * swap #660 #50 */
: hz ( tf- ) push #1000 #1193 pop */
: osc ( tp- ) dup period ! split $42 p! $42 p!
: tone ( t- ) mute @ #0 + drop if drop ; then $4F $61 p! ms $4D $61 p! #20 ms ;
: click #1 #90 osc ;
: t #3 tn ;
: q #8 tn ;
: c #16 tn ;
: 2tone #75 q #50 q ;
: h1 #50 c #54 q #50 q #45 c #60 c ;
: h2 #40 c #45 q #50 q #50 c #45 c ;
: h3 #54 c #60 q #54 q #50 c #45 q #40 q #50 t #45 t #50 t #45 t #45 #12 tn #40 q #40 #32 tn ;
: hh
: handel h1 h2 h3 ;
: piano #55 #7 for dup q #3 #2 */ next drop ;
: cetk #6 c #10 c #8 c #4 c #6 #32 tn ;
: bomb mute @ #0 + drop if ; then $4F $61 p! #500 for #1000 i invert + split $42 p! $42 p! #1 ms next $4D $61 p! #1 #32 tn 
; handel 

\ Block 115 
\ ( Sounds : using the PC internal speaker )
\ : tempo ( in ms per 1/8 quaver )
\ : mute ( equals -1 to disable sound )
\ : period ( test only - value sent to hardware )
\ : tn ( ft- play f Hz for t * 11 ms )
\ : hz ( tf- play t ms at f Hz )
\ : osc ( tp- play t ms of period p )
\ : tone ( t- play the current tone for t ms )
\ : click ( makes a click )
\ : t ( triplet )
\ : q ( quaver )
\ : c ( crotchet )
\ : 2tone ( 2 tones )
\ : h1
\ : h2
\ : h3
\ : hh
\ : handel ( part of Handels Gavotte )
\ : piano
\ : cetk ( Close Encounters of the Third Kind )
\ : bomb ( - well sort of .... ) 

\ Block 116 
( Colourblind Editor Display ) 
#1 MagentaV state $01 MagentaV state*
: +txt white $6D emit space ;
: -txt white $6E emit space ;
: +imm yellow $58 emit space ;
: -imm yellow $59 emit space ;
: +mvar yellow $09 emit $11 emit $05 emit $01 emit space ;
: txts string $03010100 , $07060504 , $09090901 , $0F0E0D0C , ( ; )
: tx ( c-c ) $0F and txts + 1@ $0F and ;
: .new state @ $0F and jump nul +imm nul nul nul nul nul nul nul +txt nul nul +mvar nul nul nul ;
: .old state* @ $0F and jump nul -imm nul nul nul nul nul nul nul -txt nul nul nul nul nul nul ; 
here
: cb ( n-n ) #0 + 0if ; then tx 
   state @ swap dup state ! - drop if .old .new 
   state @ #0 + if dup state* ! then then ;
: cbs ( -- here ) #0 + $00 + cblind ! ; 

\ Block 117 
\
\ : state
\ : cb ( acts on a change of token type. It ignores extension tokens ) 

\ Block 118 
( Graphics demo Todo: fix this! ) empty 
#2 #22 +thru 
: htm #116 load ( html ) ; 
log1 

\ Block 119 
\ ( A graphics extension package )
\ : . ( Type ) ok ( after loading this block ) 

\ Block 120 
( added macros ) forth
: mfill #24 for cr space #5 for rand32 h. space next next ;
: matrix show black screen green mfill keypad ; 

\ Block 121 
\ ( added macros )
\ : 1+ ( increment tos )
\ : 1- ( decrement tos )
\ : @b ( fetch byte from absolute addr. )
\ : @w ( fetch word from absolute addr. )
\ : @l ( fetch long from absolute addr. )
\ : !b ( store byte in absolute addr. )
\ : !w ( store word in absolute addr. )
\ : !l ( store long in absolute addr. )
\ : matrix ( What is the Matrix? )
\ : ver ( returns the address of the CFDOS version - use as ) ver dump 

\ Block 122 
( Stack juggling + misc. )
: v- ( v-v ) push invert 1+ u+ pop invert 1+ + ;
: vn push rot less if rot pop -rot ; then -rot pop ; #2222 MagentaV pen #236986408 MagentaV bs
: vloc ( xy-a ) scrnw 2* * over + + vframe + ; 
macro
: @w $8B66 3, ;
: !w a! $00028966 3, drop ; 
forth
: point ( xy- ) pen @ swap w! ;
: at? ( -xy ) xy @ $00010000 /mod swap ;
: @r ( a-a ) 1+ dup #4 u+ @ + ;
: !r ( aa- ) 1+ dup push negate #-4 + + pop ! ;
: select ( an- ) #5 * over + @r swap @r !r ; 

\ Block 123 
\ ( Stack juggling words. small and fast. )
\ : addr -a ( absolute address )
\ : rot abc-bca ( stack pictures are best .. )
\ : -rot abc-cab ( ..described with letters, in )
\ : tuck ab-bab ( ..this case. )
\ : 2swap abxy-xyab
\ : 2over abxy-abxyab
\ : 2dup ab-abab
\ : v- v1v2 - v1-v2 ( vector subtract. )
\ : vn vv-vv ( sort vectors so x1 is less x2 )
\ : vframe -addr ( address of screen. )
\ : pen -addr ( current color. )
\ : bs -addr ( base for elements )
\ : vloc xy-a ( convert xy into addr. )
\ : point xy- ( set point at xy to current pen. )
\ : at? -xy ( return current screen location. )
\ : @r a-a ( get absolute addr from jump/call )
\ : !r aa- ( set jump/call to absolute addr. )
\ : select an- ( select call n from table a. Store it in table call 0 ) 

\ Block 124 
( new logo )
: .co $72 $6F $6C $6F $63 5* ;
: .fo $68 $74 $72 $6F $46 5* ;
: cf #27 dup at silver .co .fo #25 dup at red .co green .fo ;
: log1 show black screen text cf keypad ;
: ckb black #0 #740 at #1023 #767 box #800 #650 at #1023 #740 box ;
: grads #0 #128 for i 2* 1- rgb color dup #10 at #5 + dup #120 box next 
   iconw #21 * - #128 for #257 i 2* negate + dup #256 * + rgb color dup #10 at #5 + dup #100 box next drop ; 

\ Block 125 
\ ( New logo )
\ : log1 ( a simple text demo )
\ : ok ( the graphics demo ) 

\ Block 126 
( Circles ) #-16977 MagentaV c-cd #0 MagentaV c-ff
: point4 #4096 * swap #4 * 2dup + 2/ negate bs @ + pen @ over w! over push + pen @ over w! + pen @ over w! pop negate + pen 
@ swap w! ;
: opnts 2dup point4 2dup swap point4 ;
: d? c-cd @ ?f drop -if ; then dup invert c-cd +! 1- #1 c-ff ! ;
: cfl 1+ 1+ push pen @ swap pop 2/ for over over w! 1+ 1+ next drop drop ;
: cfl4 #4096 * swap #4 * 2dup + 2/ negate bs @ + swap 2dup cfl push + pop cfl ;
: fvrt ?f drop if cfl4 #0 c-ff ! ; then point4 ;
: fpnts 2dup c-ff @ fvrt 2dup swap cfl4 ;
: points opnts ;
: addr pop ;
: pntst addr points opnts fpnts ;
: framed pntst #1 select ;
: filled pntst #2 select ;
: circle ( rxyc- ) #0 c-ff ! pen ! #1024 * + 2* vframe + bs ! #0 swap dup negate c-cd !
: crcl less if points #1 u+ over c-cd +! d? crcl ; then points drop drop ; 

\ Block 127 
\ ( Circles )
\ : point4 ( .. all other words are internal. )
\ : points ( acts like a deferred word. )
\ : pntst ( table of calls to different point routines. Select alters ) points
\ : framed ( set ) circle ( to draw outlined circles. )
\ : filled ( set ) circle ( to draw filled circles. )
\ : circle rxyc- ( draw circle with radius ) r ( center ) xy ( in color ) c 

\ Block 128 
( lines ) 
#-1456 MagentaV ax #0 MagentaV ay #2048 MagentaV sx #2 MagentaV sy #31987278 MagentaV lbase 
macro
: lp $8B909090 , $C88BADE8 , $205A8BAD , $232B8966 , $030578C0 , $185A0302 , $03084203 , $ECE2105A , ; 
forth
: !base ( xy- ) #2048 * over + + vframe + lbase ! ;
: bline ( xy- ) abs 2* dup ay ! over 2* negate ax ! over 
   negate + swap 1+ pen @ ax a! lp drop ;
: ?xd ( vv-vv ) 2over 2over v- abs swap abs swap less 
   drop drop #-1 if 1+ then ?f drop ;
: !sy ( yn-y ) push ?f pop -if negate then sy ! bline ;
: xdom ( xyxy- ) 2swap !base #2 sx ! #2048 !sy ;
: ydom ( xyxy- ) swap 2swap swap !base swap #2048 sx ! 
   #2 !sy ;
: aline ( vv- ) ?xd if vn 2over v- xdom ; then push push 
   swap pop pop swap vn 2over v- ydom ;
: line ( xy- ) at? 2over aline at ;
: frame ( xy- ) at? 2over drop over line 2over line 2swap 
   push drop over pop line line ; 

\ Block 129 
\ ( line drawing Do Not Mess With Variables. They are indexed by lp. )
\ : lp ( macro inner loop for speed. Draws point and moves location. )
\ : !base x y -- ( set base address )
\ : bline dx dy -- ( draw a line using bresenham x dominant )
\ : ?xd v1 v2 -- v1 v2 ( set flag if line is x-dominant )
\ : !sy dy n -- dy ( store n in sy set sign to match sign of dy )
\ : xdom x y dx dy ( draw an x-dominant line )
\ : ydom x y dx dy ( draw a y-dominant line )
\ : aline v1 v2 ( draw any straight line )
\ : line x y ( draw line from current at to xy. Moves at to given xy. )
\ : frame xy- ( trace outline of rectangle with corners at and xy. Pen position is not altered. ) 

\ Block 130 
( Utils )
: xxcoy ( sf st ) $E7C1F88B , $368B560A , $B90AE6C1 , $0100 , $AD5EA5F3 , $C3AD 2,
: xrcopy ( sf sl st ) push dup push swap negate + pop swap pop over + swap for over over copy push 1- pop 1- -next drop drop 
; 

\ Block 131 
\ ( Utils )
\ : copy from to- ( copy from to block numbers. Unlike orig copy; no change to blk )
\ : rcopy first last to- ( multiple block copy routine ) 

\ Block 132 
( fillstack ) #1114112 MagentaV fstak $00 MagentaV fstakn
: fstini ( - ) $0400 block fstak ! 0 fstakn ! ;
: fpop ( -uuu ) fstak @ #3 for dup @ swap cell- next 
   fstak ! #-3 cells fstakn +! ;
: fpsh ( uuu- ) #3 for cell fstak +! fstak @ ! next 
   #3 cells fstakn +! ;
: fst? ( - ) fstakn @ ?f drop ; fstini 
macro
: 2- 1- 1- ;
: 2+ 1+ 1+ ; 
forth
: 5drop ( uuuuu- ) drop drop drop drop drop ;
: rtre ( a-n ) #2048 #1 - and negate #2048 + ;
: enstak ( dlrlr-dlrlr ) 2- #4 pick dup #3 pick + over 
   #3 pick + fpsh over #4 pick negate + 2+ drop 
   -if #4 pick negate dup #3 pick + 

\ Block 133 
\ ( fillstack: stack of spans to fill. )
\ : fstini ( initialize )
\ : fpop ( pop the next element from the stack )
\ : fpsh ( push element on the stack )
\ : fst? ( set 0 flag if empty )
\ : 2- ( screen pixels are 2 bytes. )
\ : 2+
\ : 5drop ( unload forth stack. )
\ : rtre a-n ( return remaining to right screen edge. )
\ : enstak dlrlr-dlrlr ( push a span or element onto the stack. Also push a left hand direction reversal and a right hand 
\ reversal if needed. ) 

\ Block 134 
( area filling ) #25702 MagentaV tfc #14660 MagentaV fc
: pset ( a-f ) dup dup w@ $FFFF and tfc @ negate + drop 
   if drop 0 ; then fc @ swap w! 0 1+ ;
: bcup ( a-a ) dup #2048 #1 - and 2- begin -if drop ; then 
   push 2- pset drop pop if 2- *end then drop 2+ ;
: ispan pset if ; then push enstak pop ;
: xgr dup negate #3 pick + drop ;
: nispan ( dlrlx- ) xgr -if 5drop pop pop pop drop drop 
   drop ; then pset if push nip dup pop then ;
: dosp ( dlrlx-dlrlxi ) jump nispan ispan ;
: sha2 over rtre begin ( dlrlxic ) -if drop ; then push 
   dosp #2 u+ pop 2- end
: sha1 ( dlr- ) over pset over ( dlrxil ) if bcup ( dlrxil ) then 
   swap push swap 2+ pop ( dlrlxi ) sha2 ?f drop 
   if enstak then 5drop ;
: sha begin fst? if fpop sha1 *end then ;
: fsln ( a-lr ) dup bcup swap dup rtre 
   begin -if drop ; then push pset drop if 
   2+ pop 2- *end then pop drop 2- ;
: afill ( xyc- ) fstini fc ! vloc dup w@ $FFFF and tfc ! 
   fsln over over #-2048 u+ #-2048 + #-2048 -rot fpsh 
   #2048 u+ #2048 + #2048 -rot fpsh sha ;
: afill drop drop drop ; 

\ Block 135 
\ ( area filling )
\ : pset a-0/1 ( set pixel at a, if pixel equals tfc. Return 0 if not, 1 if pixel was set. )
\ : bcup a-a ( adjust a until left edge is found. Limited to screen edge. )
\ : ispan ( stack if the right edge is found. )
\ : xgr ( Set neg flag if x is greater then parent-r )
\ : nispan ( exit if beyond right edge of span, else start a new span. )
\ : dosp dlrlx - dlrlxi ( jump table. )
\ : sha2 ( let x go over each pixel and set it or start/end new spans. )
\ : sha1 ( starting at left edge, find the new left edge and init x to next pixel. stack if run into right screen edge while 
\ in span. )
\ : sha ( pop the next span and color it. )
\ : fsln a-lr ( Starting at screen address a, find the left edge and right edge of the seed line. Color it in the proces 
\s. )
\ : afill xyc ( starting with screen location xy, and color c, fill the color found there with c until the color found change 
\s. ) 

\ Block 136 
( random ) #-1896373196 MagentaV rsav #-526774649 MagentaV rseed
: rand ( -- ) time rsav ! $E09A0E87 rseed ! ;
: ror ( u-u ) $D3ADC88B , $C3C8 2,
: random ( w-w ) push rseed @ #0 #32 for 2* swap 2* swap -if rsav @ xor then next nip #15 ror dup rsav ! abs pop mod abs 
; rand
: tt $0100 random ; 

\ Block 137 
\ ( random )
\ : rand - ( set random variables )
\ : ror nm-n ( rotate n m times right )
\ : random n-0..n-1 ( return a random number range 0..n-1 limited to a 16 bit number. ) 

\ Block 138 
( demos )
: xlate #384 + #512 u+ ;
: xat xlate at ;
: xline xlate line ;
: 4lines over #0 xat #0 over xline over - #0 xline negate #0 swap xline #0 xline ;
: art #70 for #71 i - #5 * i #5 * 4lines next ;
: radius #8 ;
: lrc push dup dup + negate pop + random + ;
: shade 2over #2 + 2over drop #3 + #0 circle circle ;
: dotty filled #100 for radius random dup #397 lrc #621 + over #176 lrc #121 + $FFFF random shade next ;
: blbx black #6 #121 at #404 #299 box ; #-17 MagentaV xyzz
: fillit #-1 xyzz +! xyzz @ #200 + drop -if blbx 0 xyzz ! then framed #3 for #8 random #2 + dup #398 lrc #6 + over #178 lrc 
#121 + $FFFF circle next 
; #6 #210 $FFF0 random afill ; 

\ Block 140 
( new logo 2 )
: lnes framed #20 for i 2* #40 + #250 #584 $FF07 circle next filled #30 #250 #584 $F800 circle framed $FFFF pen ! #620 #120 
at #1020 #300 frame #5 #120 at #405 #300 frame ;
: ok show black screen grads lnes text cf dotty fillit ckb keypad ; ( ok ) 

\ Block 141 
\ ( New logo )
\ : log1 ( a simple text demo )
\ : ok ( the graphics demo ) 

\ Block 142 
( html0 ) #80 load #2222119 MagentaV h-dd #0 MagentaV ppt macro
: 2/s ?lit $E8C1 2, 1, ; forth
: temit h-dd @ !b #1 h-dd +! ;
: tspc $20 temit ;
: .dc ?f #1 -if - then swap abs
: dcl #10 /mod swap $30 + push ?f 0if drop ?f drop -if $2D temit then pop temit ; then dcl pop temit nop ;
: .hx $39 over #15 and $30 + less nip if $27 + then push #4 2/s 0if drop pop temit ; then .hx pop temit nop ;
: strt dup @b $FF and if temit 1+ strt ; then drop drop ;
: str: pop strt ;
: header str: $6D74683C , $3C0A3E6C , $6B6E696C , $6C657220 , $7974733D , $6873656C , $20746565 , $65707974 , $6574223D , 
$632F7478 , $20227373 , $66657268 , $3D 1, $6C6F6322 , $6F66726F , $2E687472 , $22737363 , $703C0A3E , $0A3E 3,
: trailer str: $74682F3C , $0A3E6C6D , $00 1, 

\ Block 143 
\ ( html0. Block 80 has ascii conversion tables. )
\ : h-dd ( data destination. ) ppt ( pre- parsed type. )
\ : 2/s ( macro, right shift by n. )
\ : temit c- ( emit char to target. )
\ : tspc ( emit space )
\ : .dc n- ( signed decimal print. Recursive! )
\ : dcl ( dec print loop. )
\ : .hx n- ( unsigned hex print. Also recursive. Both routines have no leading zeroes. )
\ : strt a- ( Print bytes from address until first null byte. )
\ : str: ( Output what follows up to null byte. )
\ : header ( Lay down html header to display blocks. The header is very minimal. It expects colorforth.css in the same direct 
\ory. )
\ : trailer ( Closing html stuff. ) 

\ Block 144 
( html1 )
: .code 1- drop -if ; then str: $6F632F3C , $003E6564 ,
: .all str: $646F633C , $6C632065 , $3D737361 , $00 1,
: same? ppt @ over ppt ! swap over - 1+ + drop ;
: comn same? 0if drop tspc pop drop ; then .code .all ;
: .def str: $3E666564 , $20 2,
: .com #2 comn str: $3E6D6F63 , $20 2,
: .chx #3 comn str: $3E786863 , $20 2,
: .exe #4 comn str: $3E657865 , $20 2,
: .xhx #5 comn str: $3E786878 , $20 2,
: .cpm #6 comn str: $3E6D7063 , $20 2,
: .var #7 comn str: $3E726176 , $20 2,
: .txt #8 comn str: $3E747874 , $20 2,
: .txc #9 comn str: $3E637874 , $20 2,
: .tac #10 comn str: $3E636174 , $20 2, 

\ Block 145 
\ ( html1 )
\ : .code n- ( output /code in brackets if n is larger then 0. )
\ : .all ( common part to start a new code tag. )
\ : same? n-o ( set ppt to the new type. Return the old type with flags set from comparison. )
\ : comn n- ( if this is a new tag, close prev tag and print common part. If not: print space AND EXIT CALLER )
\ : .def ( Each of these words correspond to a )
\ : .com ( .. code tag as defined in colorforth.css )
\ : .chx ( .. The numbers are positional, and bare )
\ : .exe ( .. no correspondence to the pre parsed )
\ : .xhx ( .. types. They will output if a change )
\ : .cpm ( .. in tag is required. Comn will exit )
\ : .var ( .. by doing a pop-drop if the tag is the )
\ : .txt ( .. same. )
\ : .txc
\ : .tac 

\ Block 146 
( html2 )
: .str ch if temit .str ; then drop drop ;
: bs1 #0 ppt ! str: $3E72683C , $6C627B0A , $206B636F , $00 1,
: bs2 str: $643C0A7D , $63207669 , $7373616C , $786F623D , $0A3E 3,
: bend ppt @ .code str: $69642F3C , $000A3E76 ,
: .br 1- drop -if ; then str: $3E72623C , $0A 2,
: pp0 .str ;
: pp1 .exe .str ;
: pp3 ppt @ dup .code .br #1 ppt ! .all .def .str ;
: pp4 .com .str ;
: pp7 .cpm .str ;
: pp9 .txt .str ;
: ppa .txc .str ;
: ppb .tac .str ;
: ppc .var .str 1+ dup @ .com .dc ; 

\ Block 147 
\ ( html2 )
\ : .str n- ( Unpack n and print as ascii. )
\ : bs1 ( clear the type and print html stuff for the start of a block. )
\ : bs2 ( second half of block header. )
\ : bend ( Block end html stuff. )
\ : .br n- ( Html line break, if n larger then 0 )
\ : pp0 ( The preparsed words in a block are )
\ : pp1 ( .. printed by the ppn words. Eg pp0 is )
\ : pp3 ( .. word continuation pp1 is for executed )
\ : pp4 ( .. words, etc. They unpack and print. )
\ : pp7 ( .. They also print html tags. )
\ : pp9
\ : ppa
\ : ppb
\ : ppc 

\ Block 148 
( html3 )
: dbn push 1+ dup @ pop ?f drop ;
: sln dup 2/ 2/ 2/ 2/ 2/ swap #16 and drop ;
: xnb if .xhx .hx ; then .exe .dc ;
: cnb if .chx .hx ; then .com .dc ;
: pp2 dbn xnb ;
: pp5 dbn cnb ;
: pp6 sln cnb ;
: pp8 sln xnb ;
: ppdo jump pp0 pp1 pp2 pp3 pp4 pp5 pp6 pp7 pp8 pp9 ppa ppb ppc ;
: index dup #15 and dup push or pop ;
: dblk dup bs1 .dc bs2 block begin dup @ ?f 0if drop drop bend ; then index ppdo 1+ end
: hbuf #2000 block ;
: html hbuf #4 * h-dd ! header swap over for over i - 1+ + over + dblk next drop drop trailer hbuf h-dd @ #3 + #4 / over 
- 1+ + #3 for tspc next ; 

\ Block 149 
\ ( html3 )
\ : dbn an-an ( Fetch next word. Set hex flag. )
\ : sln n-n ( Make full word and set hex flag. )
\ : xnb n- ( print n as hex/dec executed number. )
\ : cnb n- ( print n as hex/dec compiled number. )
\ : pp2 an-a ( A double executed number. )
\ : pp5 an-a ( A double compiled number. )
\ : pp6 n- ( A single compiled number. )
\ : pp8 n- ( A single executed number. )
\ : ppdo ( Table of words. The index is the pre- parsed type type. )
\ : index n-ni ( extract index from n. )
\ : dblk b- ( print block b in html. )
\ : hbuf -a ( start of buffer. )
\ : html bn-al ( Output n blocks starting with block b in html. Leaves addr and length on the stack, so it can be saved using 
\ ) file put ( on a floppy. ) 

\ Block 150 
( html3 )
: dbn push 1+ dup @ pop ?f drop ;
: sln dup 2/ 2/ 2/ 2/ 2/ swap #16 and drop ;
: xnb if .xhx .hx ; then .exe .dc ;
: cnb if .chx .hx ; then .com .dc ;
: pp2 dbn xnb ;
: pp5 dbn cnb ;
: pp6 sln cnb ;
: pp8 sln xnb ;
: ppdo jump pp0 pp1 pp2 pp3 pp4 pp5 pp6 pp7 pp8 pp9 ppa ppb ppc ;
: index dup #15 and dup push or pop ;
: dblk dup bs1 .dc bs2 block begin dup @ ?f 0if drop drop bend ; then index ppdo 1+ end
: hbuf #2000 block ;
: html hbuf #4 * h-dd ! header swap over for over i - 1+ + over + dblk next drop drop trailer hbuf h-dd @ #3 + #4 / over 
- 1+ + #3 for tspc next ; 

\ Block 151 
\ ( html3 )
\ : dbn an-an ( Fetch next word. Set hex flag. )
\ : sln n-n ( Make full word and set hex flag. )
\ : xnb n- ( print n as hex/dec executed number. )
\ : cnb n- ( print n as hex/dec compiled number. )
\ : pp2 an-a ( A double executed number. )
\ : pp5 an-a ( A double compiled number. )
\ : pp6 n- ( A single compiled number. )
\ : pp8 n- ( A single executed number. )
\ : ppdo ( Table of words. The index is the pre- parsed type type. )
\ : index n-ni ( extract index from n. )
\ : dblk b- ( print block b in html. )
\ : hbuf -a ( start of buffer. )
\ : html bn-al ( Output n blocks starting with block b in html. Leaves addr and length on the stack, so it can be saved using 
\ ) file put ( on a floppy. ) 

\ Block 152 
( simpler and slower bresenham line drawing. For reference. ) #-360 MagentaV ax #0 MagentaV ay #2 MagentaV sy #0 MagentaV sw 
: bpoint push 2dup sw @ ?f drop if swap then point pop ;
: bline abs 2* dup ay ! over 2* negate ax ! over negate + swap 1+ for bpoint ?f +if sy @ u+ ax @ + then ay @ + push #1 u+ 
pop next drop drop drop ;
: ?xd 2over 2over v- abs swap abs swap less drop drop #-1 if 1+ then ?f drop ;
: !sy push ?f pop -if negate then sy ! bline ;
: xdom #0 sw ! #1 !sy ;
: ydom #1 sw ! #1 !sy ;
: aline ?xd if vn 2over v- xdom ; then push push swap pop pop swap vn 2over v- ydom ; 

\ Block 154 


\ Block 155 
\ ( fillstack: stack of spans to fill. )
\ : fstini ( initialize )
\ : fpop ( pop the next element from the stack )
\ : fpsh ( push element on the stack )
\ : fst? ( set 0 flag if emtpy. )
\ : pick ( copy n from the stack. )
\ : 2- ( screen pixels are 2 bytes. )
\ : 2+
\ : 5drop ( unload forth stack. )
\ : rtre a-n ( return remaining to right screen edge. )
\ : enstak dlrlr-dlrlr ( push a span or element onto the stack. Also push a left hand direction reversal and a right hand 
\ reversal if needed. ) 

\ Block 160 
( Timing ) empty macro
: out $E1E6 2, ; forth
: tare time invert #1000 for next time + ;
: tare+ time invert push #1000 for dup next c pop time + ;
: test +     s #1000 for out next time + ; ( next 3 loop 5.7 /next 2 /swap 25 swap 7.2 ) macro
: c! $C88B 2, drop here ;
: loop $49 1, $75 1, ( e2 ) here invert + 1, ; forth
: try time invert #1000 c! loop time + ; 

\ Block 162 
( Spy ) empt $03F8 #54 load init
: ry #5 r p@ ; nload init
: buffer #2000 block ; #2000 #1 wipes #0 MagentaV buf #0 buf !
: b! swap $FF and + buf @ buffer + ! #1 buf +! ;
: dev r2 if dup xmit $0100 b! dev ; then ;
: pc ?rcv if dup x2 0 b! pc ; then ;
: relay s2 st s2! st! dev pc ;
: .1 $0F and digit ;
: .byte dup $10 / .1 .1 ;
: traffic text buffer buf @ #1 max #400 min for dup @ green $0100 ? if red then .byte #1 + next drop ;
: ok show black screen relay traffic keyboard ;
: k show black screen relay keyboard ;
: q #6000 for relay next ;
: test st! st ; #84 load 

\ Block 164 
( Serial 2 )
: r $02F8 + ;
: b/s $83 #3 r p! 9600 #262 #0 r p! #0 #1 r p! #3 #3 r p! ;
: init b/s ( 16550 ) #1 #2 r p! #0 #4 r p! ;
: x2 #5 r p@ $20 and drop if #0 r p! ; then x2 ;
: c2 #6 r p@ $30 and $30 or drop if c2 ; then x2 ;
: s2 #6 r p@ xbits ;
: s2! #4 r p! ;
: r2 #5 r p@ #1 and drop if #0 r p@ ; then ; 

\ Block 166 
( Dynapulse 200m )
: send pop swap for dup 1@ x2 #1 + next drop ;
: reset #2 send $2323 ,
: 1st #12 send $37269A12 , $39027AFD , $23C75680 , 

\ Block 168 
( Test sidt and lidt ) 
#7168 MagentaV vidt sidt vidt !
: resi cli vidt @ lidt ; 

\ Block 169 
\ ( This block is used by the next block as the interrupt vector table. ) 

\ Block 170 
( Interrupts ) macro
: 1ld ( n ) ?lit $B9 1, , ;
: p! ( na ) a! $EE 1, drop ;
: 2push $5250 2, ;
: 2pop $585A 2, ;
: forth: 2push $00BE5651 3, ivec $0100 + a, ;
: ;forth $595E 2, 2pop ;
: clear $20E620B0 , ;
: 8clear $A0E620B0 , $20E6 2, ;
: i; $CF 1, ; forth
: interrupt ( n ) 2* 2* 2* ivec + here $FFFF and $00080000 + over ! here $FFFF0000 and $8E00 + swap #4 + ! ;
: ifill ( an ) for dup interrupt #1 + next drop ; $00 $70 ifill
: ignore i; $20 $08 ifill
: ignore 2push clear 2pop i; $28 $08 ifill
: ignore 2push 8clear 2pop i; $00 interrupt
: 0div $7FFFFFFF 1ld i; 

\ Block 171 
\
\ : idt -a ( table of 2-word interrupts. Edit convenient block number )
\ : 1ld n ( load register 1 with literal )
\ : lidt ( load interrupt descriptor table from byte address on stack )
\ : 2push ( save registers 0 and 2 )
\ : 2pop ( restore 2 and 0 )
\ : forth: ( save registers used by Forth )
\ : ;forth ( restore registers used by Forth )
\ : clear ( store 20 to port 20 to clear irq 0-7 )
\ : 8clear ( also 20 to port a0 to clear irq 8-f )
\ : i; ( return from interrupt - restore flags )
\ : !idt b ( execute lidt )
\ : interrupt n ( construct interrupt to ) here. ( Avoid yellow-green literal with red comment )
\ : ifill an ( n entries in default interrupt table )
\ : ignore ( clear ) ( grey = $01644001  ) ( interrupt. Doesnt clear the device )
\ : 0div ( make divisor +infinity, quotient 0 ) 

\ Block 172 
( Admtek Comet An983b ) macro
: align here #7 and #3 xor drop if nop align ; then ; forth
: array pop 2/ 2/ ;
: us ( n ) khz @ #1000 #3 * / * for next ;
: r ( n-a ) $DB000000 + 2/ 2/ ;
: rom ( a-n ) $A4 + r @ ;
: 3rom ( nnn ) #4 rom #0 rom dup #16 for 2/ next swap ;
: reset #1 $00 r ! #1000 us ;
: frag #0 , $02000000 , $00 , here #4 + , ;
: tx align array frag frag frag frag frag frag
: n tx #1 + ;
: a tx #2 + ; #16 MagentaV f
: fr! f @ + ! ;
: first ( an ) #0 f ! $20000000 or
: send ( an ) $01000000 or n fr! a fr! $80000000 tx fr! #4 f +! ;
: last ( an ) $42000000 or send #1 us
: poll #-1 $08 r ! ; 

\ Block 173 
\
\ : array -a ( returns word-aligned address in dictionary )
\ : us n ( delay n microseconds. Edit cpu clock rate )
\ : r n-a ( word address of register. Edit base address from ) north ( PCI device configuration )
\ : rom a-n ( fetch 2 bytes of ethernet id )
\ : 3rom -nnn ( 3 byte-pairs of id. )
\ : reset ( controller )
\ : tx -a ( transmit descriptor ring )
\ : n -a ( fragment length/control )
\ : a -a ( fragment address )
\ : send an ( fragment into descriptor queue )
\ : first an ( fragment. )
\ : last an ( fragment. Start transmission ) 

\ Block 174 
( Receive ) #281880 MagentaV rxp
: rx align array $80000000 , $01000600 , $2000 block #4 * dup , here #4 + , $80000000 , $01000600 , $0600 + , rx #4 * ,
: init reset rx #2 * 2* $18 r ( receive ) ! #1 us tx #2 * 2* $20 r ( transmit ) ! #1 us $00202002 ( start ) $30 r ! #1 us 
$00010040 $38 r ! sti #-1 $28 r ! ;
: link #3 + @ 2/ 2/ ;
: own? @ #0 or drop ;
: /int rxp @ $80000000 over ! link own? -if #-1 $28 r ! then ;
: rcvd rx nop
: wait dup own? -if link wait ; then dup rxp ! #2 + @ ;
: reg dup r @ h. space #2 h.n cr ;
: regs $B8 reg $A0 reg $98 reg $94 reg $78 reg $60 reg $48 #10 for dup reg #-8 + next drop ;
: ok show $00400000 rgb color screen text regs keypad ;
: rx1 $2000 block dump ;
: rx2 $2000 block $0180 + dump ; ok 

\ Block 175 
\
\ : rx -b ( receive descriptor ring )
\ : init ( ialize controller. Set tx/rx address/on and perfect match )
\ : link a-b ( next link in descriptor ring )
\ : own? a ( is this descriptor owned? )
\ : /int ( give up ownership of received packet , clear interrupt if no packet remains )
\ : rcvd -a ( return address of recieved packet )
\ : wait -b ( till packet received )
\ : reg a ( display register and address )
\ : regs ( display interesting registers )
\ : ok ( diagnostic display ) 

\ Block 176 
( App: Ethernet ) empty 
( interrupts ) #170 load 
( hardware interface ) #172 load #174 load macro
: w $66 1, ;
: w@ $8B 2, ;
: w! $0289 2, drop ;
: *byte $C486 2, ; forth
: n@ w w@ $FFFF and *byte ;
: 2! a! w w! ;
: n! a! *byte w w! ;
: n, *byte 2, ;
: string pop ;
: packet string #-1 dup , 2, 3rom 2, 2, 2, #0 n,
: length ( n ) packet #12 + n! ;
: broadcast #-1 dup dup packet nop
: 3! swap over 2! #2 + swap over 2! #2 + 2! ;
: ethernet ( n ) length packet #14 first ;
: +ethernet ( -a ) rcvd #14 + ; fixthis 
#2 #16 +thru breakhere ( todo fix this ) 
$2A interrupt
: serve forth: receive /int 8clear ;forth i; init ok discover 

\ Block 177 
\
\ : empty ( redefined to disable interrupts )
\ : w ( 16-bit prefix )
\ : w@ b-n ( fetch 16-bits from byte address )
\ : w! nb ( store 16-bits )
\ : *byte n-n ( swap bytes 0 and 1 )
\ : n@ b-n ( fetch 16-bit network-ordered number )
\ : 2! nb ( store 16-bit number )
\ : n! nb ( store 16-bit number in network order )
\ : n, n ( compile 16-bit number in network order )
\ : string -b ( returns byte address )
\ : packet -b ( ethernet packet header )
\ : dest -b ( destination field in packet )
\ : src -b ( source field )
\ : length n ( store length into packet )
\ : 3! nnnb ( store 3-word MAC )
\ : ethernet n ( send header with type/length )
\ : @ethernet -b ( return payload address of received packet ) 

\ Block 178 
( ARP for a single correspondent ) macro
: move ( sdn ) $C189 2, drop $00C78957 3, drop $00C68956 3, $A4F3 2, $5F5E 2, drop ; forth
: . ( n ) 1, ;
: message string $01 n, $0800 n, $06 . $04 . $01 n,
: me 3rom 2, 2, 2, ( IP ) #0 . #0 . #0 . #0 .
: to #0 #0 #0 2, 2, 2, ( IP ) #0 . #0 . #0 . #0 .
: sender #8 + ;
: target #18 + ;
: dir #6 + ;
: ip #6 + w@ ;
: ar ( n ) message dir n! $0806 ( ARP ) ethernet message #28 last ;
: arp cli broadcast #1 ar sti ;
: -arp ( b-b ) dup #-2 + n@ $0806 or drop if ; then pop drop
: me? dup target ip message sender ip or drop if ; then dup sender packet #6 move
: query? dup dir n@ #1 or drop if ; then sender message target #10 move #2 ar ; 

\ Block 179 
\ ( Set ip addresses with Edit. Normal order, net bytes first )
\ : move sdn ( move n bytes from source to destination. Register 1 is used, 6 and 7 are saved )
\ : . n ( compile byte. Resembles URL punctuation )
\ : message -b ( 28-byte string )
\ : me ( comment marking my mac/ip address )
\ : to ( comment marking correspondent )
\ : sender
\ : target
\ : dir -b ( fields in either ) message ( or received message )
\ : ip b-n ( fetch ip address )
\ : ar n ( send query 1, or reply 2 )
\ : arp ( broadcast query )
\ : -arp b-b ( return if not ARP. Otherwise process and skip out. )
\ : me? b ( return if broadcast not for me. Save sender only in packet )
\ : query? b ( if a request, reply ) 

\ Block 180 
( ipv4 )
: header align string $4500 n, #0 n, #1 n, #0 n, $FF11 n, #0 n, #0 , #0 ,
: length ( n ) header #2 + n! ;
: +id header #4 + dup n@ #1 + swap n! ;
: -sum for dup n@ u+ #2 + next drop dup $00010000 / + invert ;
: sum header #10 + n! ;
: checksum 0 sum #0 header #10 -sum sum ;
: source header #12 + ;
: destination header #16 + ;
: ip ( n-n ) dup #20 + $0800 ethernet length +id checksum header #20 send ;
: +ip dup #-2 + n@ $0800 or drop if pop ; then #20 + ; 

\ Block 181 
\ ( Set ip addresses with Edit. Normal order, net bytes first )
\ : header -a ( 40-byte ipv6 header )
\ : length n ( store 2-byte length in header )
\ : dest -a ( 4-byte destination ip address )
\ : src -a ( source ip )
\ : ip n ( send ip header embedded in ethernet packet )
\ : +ip b-b ( skip out if not IP. Otherwise return payload address ) 

\ Block 182 
( UDP )
: xid 3rom + + ;
: b@ ( b-n ) w@ $FF and ;
: header string xid n, #0 n, #8 n, #0 n, #0 n,
: length ( n ) #8 + header #4 + n! ;
: port header #2 + n! ;
: from? over #-8 + n@ or drop ;
: udp ( n ) dup #8 + ip length ;
: +udp ( b-b ) dup #-11 + b@ #17 or drop if pop ; then #8 + ; 

\ Block 183 
\
\ : b@ b-n ( fetch byte )
\ : header -a ( 8-byte udp header )
\ : length n ( store length in header )
\ : port p ( set destination port )
\ : from? ap ( udp packet from port ) p ( ? )
\ : udp n ( send ip header for n-byte packet )
\ : +udp b-b ( skip out if not UDP. Otherwise return payload address ) 

\ Block 184 
( DNS resolver ) $0CF42A44 MagentaV server #1671948608 MagentaV host
: msg string #0 , #1 n, #0 2, #0 , #1 n, #1 n,
: ptr? dup n@ $C000 and $C000 or drop ;
: skip ptr? if dup b@ if + #1 + skip ; then drop #1 + ; then #2 + ;
: length dup negate swap skip + ;
: 4! a! w! ;
: query server @ destination 4! #53 port dup length dup #16 + udp drop header #8 send msg #12 send send msg #12 + #4 last 
;
: answer dup #12 + skip #4 + swap #6 + n@ ;
: resolve ( a-h ) #0 host ! query
: wait host @ #0 or if ; then drop wait ;
: rr+ #8 + dup n@ + #2 + ;
: -dns #53 from? if ; then pop drop answer
: rr #-1 + -if #-1 host ! ; then swap skip dup n@ #1 or drop if rr+ swap rr ; then
: address #10 + dup w@ host ! ; 

\ Block 185 
\ ( Assumtions )
\ : 1 ( a response contains one entry in the question section )
\ : 2 ( the first address in the answer section, if any, sufficiently resolves the query )
\ : server ( name server )
\ : host ( the resolved IP address )
\ : skip a-b ( skip past a domain field )
\ : length a-n ( length of a domain in bytes )
\ : query a- ( send DNS query to the DNS server )
\ : answer a-bn ( give the answer section and the number of resource records )
\ : resolve a-h ( resolve domain name to host address )
\ : wait -h ( wait for a response from the server )
\ : rr+ a-b ( skip a resource record )
\ : -dns ( dns packet recieved , search for address )
\ : rr a-b ( process resource record )
\ : address a-b ( set the host address ) 

\ Block 186 
( Domain names ) #62 load macro
: 1! a! $0288 2, drop ;
: interp qdup $F889 2, ; forth
: word ch if 1, #1 u+ word ; then drop drop ;
: . here #0 1, interp #0 over @ #-16 and word #1 u+
: words over @ $0F ? if drop nip swap 1! ; then word #1 u+ words ;
: end #0 1, ;
: cf string . ( www ) . ( colorforth ) . ( com ) end
: google string . ( www ) . ( google ) . ( com ) end
: none string . ( none ) end 

\ Block 187 
\
\ : 1! xa- ( write byte at byte address )
\ : interp -a ( word address of next word to be interpreted )
\ : word w- ( compile packed word as ASCII characters )
\ : . ( compile counted ASCII string )
\ : words an- ( compile extentions words as ASCII )
\ : end ( of domain )
\ : none ( test of a non-existant domain ) 

\ Block 188 
( DHCP client )
: fill for #0 , next ;
: msg align string $00060101 , xid , #5 fill 3rom 2, 2, 2, #0 2, #50 fill $6382 n, $5363 n, $00010135 3, $06030237 , #12 
1, . ( colorforth ) $FF 2, #0 , $3204 n, #0 , $FF 1,
: eq over over or drop ;
: skip over #1 + b@ #2 + u+ ;
: find over b@ if eq if $FF or if drop skip find ; then then drop drop #2 + ; then drop #1 u+ find ;
: your #16 + w@ ;
: ack dup #6 find w@ server ! #3 find w@ message target #6 + 4! your dup source 4! message sender #6 + 4! #1 ar ;
: -dhcp #67 from? if ; then dup #4 + w@ xid or drop if ; then dup #240 + dup #53 find w@
: type #2 or if #7 or drop if ack then drop ; then drop
: offer #54 find w@ msg #261 + 4! your msg #267 + 4!
: request #272 $3604 $0103 msg #241 + n!
: bootp msg #259 + n! broadcast #-1 destination 4! #67 port udp header #8 send msg swap last ;
: discover #260 $FF00 bootp ; 

\ Block 189 
\
\ : xid -v ( a unique identifier used in all DHCP correspondence with this client )
\ : fill n ( fill ) n ( words )
\ : msg ( the DHCP message , both discover and request are contained , discover is ends at ) $FF 2,
\ : eq xy-xy ( test equality )
\ : skip at-bt ( skip DHCP option )
\ : find at-b ( find option of type ) t ( in option list )
\ : your a-h ( IP address )
\ : ack ao ( server acknowledge , assign your IP , router IP , and DNS server IP )
\ : -dhcp a ( receive DHCP packet with ) xid
\ : type aot ( recieve offer ) 2 ( or ack ) 5
\ : offer ao ( recieved an offer , send a request )
\ : request ( request the offered parameters )
\ : bootp nt ( send a discover or request message )
\ : discover ( broadcast a discover message ) 

\ Block 190 
( ICMP )
: header string $0800 n, $00 n, $00 ,
: icmp dup #-34 + b@ #1 or drop if ; then ;
: ping #8 ip header #8 last ; 

\ Block 191 
\ ( Client can get or put blocks to server )
\ : payload n-bn ( 2 bytes were appended to UDP header for block number )
\ : +put nn ( send block number. Append block as last fragment. Packet length distinguishes two messages )
\ : it b ( move 1024 bytes from packet to offset block )
\ : -got b-b ( if a 2-byte message, return. Otherwise move block to archive - 300+ - and skip out )
\ : receive ( check and decode received packet. ) +test ( returns if true, ) -test ( returns if false. Otherwise they ) pop 
\ ( - skip-out - return from ) receive. ( Resulting stack need not be empty, since ) /forth ( will restore pre-interrupt 
\ stack. ) pop ( must be in a word called by ) receive, ( it cant be nested )
\ : +get b ( send requested block from archive )
\ : get n ( send block number to request. Interrupt disabled lest reply interfer )
\ : put n ( send block )
\ : archive ( send blocks 0-161 - 9 cylinders ) icmp dhcp 

\ Block 192 
( Blocks to/from server )
: payload ( n-bn ) header #8 + n! header #10 ;
: +put ( nn ) #1026 udp over payload send + block 2* 2* #1024 last ;
: it ( b ) dup #2 + swap n@ #300 + block 2* 2* #1024 move ;
: -got ( b-b ) dup #-4 + n@ #2 #8 + or drop if it pop ; then ;
: receive +ethernet -arp +ip +udp -dns -dhcp -got
: +get ( b ) n@ #300 +put ;
: ... ( interrupt-protect words that transmit )
: get ( n ) cli #2 udp payload last sti ;
: put ( n ) cli #0 +put sti ;
: archive #161 for i put #1000 us -next ; 

\ Block 193 
\ ( Client can get or put blocks to server )
\ : payload n-bn ( 2 bytes were appended to UDP header for block number )
\ : +put nn ( send block number. Append block as last fragment. Packet length distinguishes two messages )
\ : it b ( move 1024 bytes from packet to offset block )
\ : -got b-b ( if a 2-byte message, return. Otherwise move block to archive - 300+ - and skip out )
\ : receive ( check and decode received packet. ) +test ( returns if true, ) -test ( returns if false. Otherwise they ) pop 
\ ( - skip-out - return from ) receive. ( Resulting stack need not be empty, since ) /forth ( will restore pre-interrupt 
\ stack. ) pop ( must be in a word called by ) receive, ( it cant be nested )
\ : +get b ( send requested block from archive )
\ : get n ( send block number to request. Interrupt disabled lest reply interfer )
\ : put n ( send block )
\ : archive ( send blocks 0-161 - 9 cylinders ) icmp dhcp 

\ Block 194 
( Format floppy ) empt forth #1 MagentaV hd
: array pop 2/ 2/ ;
: com align array $1202004D , $6C 2,
: done $03F4 a! p@ $D0 or drop if done ; then ;
: byte ( n ) ready p! ;
: sectors ( nn-n ) #18 for over byte hd @ byte dup #18 mod #1 + byte #2 byte #1 + next drop ;
: head ( nn-n ) dup hd ! $0400 * $1202004D + com ! seek com #6 command dup 2* - #1801 + sectors done ;
: cylinders ( n ) #0 swap for #0 head #1 head #1 + next stop drop ;
: format #12 cylinders ; 

\ Block 195 
\ ( Increase speed from 2 cylinders/s to 3 )
\ : array -a ( return next word address )
\ : com -a ( address of command string )
\ : done ( wait till last sector formatted. Till ready to read )
\ : byte n ( send byte to fdc when ready )
\ : sectors nn-n ( send 4 format bytes to each of 18 sectors. Sector number from 1 to 18 )
\ : head nn-n ( set head number. Issue seek and format commands. Starting sector number depends on cylinder, allowing 2 sector 
\ times to step heads. Cylinder 1: 17 18 1 2 ... 16. 1801 + adjusts for 1s complement and for unsigned mod )
\ : cylinders n ( format both heads of each cylinder, starting at 0 )
\ : format ( standard number of cylinders. Smaller is faster ) 

\ Block 196 
( Hard disk ) empt macro ( use this at your own ) risk
: 2/s ?lit $F8C1 2, 1, ;
: p!+ $42EE 2, ;
: 1! $91 1, drop ;
: insw 1! $97 1, $006DF266 3, $97 1, ;
: outsw 1! $96 1, $006FF266 3, $96 1, ; forth
: 2dup over over ;
: bsy $01F7 p@ $80 and drop if bsy ; then ;
: rdy ( -n ) $01F7 p@ #8 and drop if $01F0 a! #256 ; then rdy ;
: sector $01F3 a! swap p!+ #8 2/s p!+ #8 2/s p!+ #8 2/s $E0 or p!+ drop p!+ drop 2* 2* ;
: read ( an ) $20 sector #256 for rdy insw next drop ;
: write ( an ) bsy $30 sector #256 for rdy outsw next drop ; nload 

\ Block 198 
( boot: 3f fat0: 5f fat1: 25a5 dir: 2 cl forth: 8e6d cl )
: reg dup p@ $FF and #2 h.n space #3 h.n cr ;
: regs #7 for i $01F0 + reg -next ;
: ok show blue screen text regs keyboard ;
: cl $20 * $4AAB + ;
: buffer $2000 block ;
: ?fort dup @ $54524F46 or drop ;
: cl0 dup #5 + @ $00010000 * swap #6 + @ #16 2/s $FFFF and or ;
: find ( -n ) buffer dup #2 cl read #256 for ?fort if #8 + *next drop ; then cl0 pop drop ;
: fort $8E6D cl ;
: +2 $8000 u+ $0100 + ;
: reads for 2dup read +2 next drop drop ;
: writes for 2dup write +2 next drop drop ;
: get buffer fort #9 reads ;
: cf! #0 fort #2 writes ; 

\ Block 200 
( Deskjet ) empty #2 +load
: nb #768 #3 * ; #4 +load
: pixels for pix next drop drop ;
: drow string $33622A1B , $622A1B4D , $5730 2,
: rpt drow #10 type drop ;
: columns for $0264 #2 wipes dup buffer #8 * #768 pixels line rpt rpt #2 + next drop ;
: res #300 2, #300 2, #2 2, ;
: esci string $306C261B , $6F2A1B4C , $1B4D312E , $3033742A , $2A1B5230 , $55342D72 , ( 32672a1b 4025736 res res res res 
) $32722A1B , $53343033 , $30722A1B , $722A1B41 , $000C4362 3,
: print esci #37 type $F0000000 #767 #1024 * #2 * + #1024 columns #6 type drop ;
: tx string $3F and if $3F or if ; then $C0 or ; then ;
: text tx map ! print ;
: it table map ! print ; 

\ Block 202 
( Printer ) macro
: p@ $EC 1, ;
: p! $EE 1, ;
: @w $8B66 3, ;
: @b $8A 2, ;
: +a $C2FF 2, ;
: bts $0010AB0F 3, drop ;
: 2/s ?lit $F8C1 2, 1, ; forth
: ready p@ $80 and if ; then ready ;
: delay for next ;
: emit $0378 a! p! +a ready +a $8D or p! #30 delay #1 or p! drop ;
: type for dup @b emit #1 + next ;
: buffer $0264 block #4 * ;
: string pop ;
: !b dup - #7 and a! dup #3 2/s bts #1 + ;
: three !b
: two !b
: one !b
: nul drop ;
: white $FFFF and dup $FFFF or drop if - then ; 

\ Block 204 
( Deskjet )
: -nb nb negate u+ ;
: bcmy string $10243800 , $3033 , $00200022 , $10000011 , $C00F , $4003 , $00 , $00 , $0008000A , $00 , $00800002 , $00 , 
$04000005 , $00 , $00 , $C0000001 ,
: ye nb #3 * u+
: all over over #3 and jump nul one two three
: ma -nb #2 2/s all ;
: cy -nb #2 2/s all ;
: bl -nb #2 2/s all ; #1050918 MagentaV map
: 6b $C618 and #3 2/s dup #3 2/s or $03C3 and dup #4 2/s or $3F and ;
: table string bcmy + @b ;
: ex map @ push ;
: pix over @w 6b ex $FF and if ye ma cy bl then drop #3 + #1024 #-2 * u+ ;
: arow string $30622A1B , $4D 1,
: trbp string $32622A1B , $00563838 3,
: trbr string $32622A1B , $00573838 3,
: color #7 type drop nb #8 / type ;
: line arow #5 type drop buffer #3 for trbp color next trbr color drop ; 

\ Block 206 
( x18 simulator ) empty macro
: 2/s ?lit $F8C1 2, 1, ; forth
: state $1FFF block ; nload
: reset r #26 for $00100000 over ! #1 + next drop $0180 mem @ ir ! $0181 pc ! $00 slot ! ;
: un. #5 for #37 emit next ;
: undef $00100000 ? if drop un. ; then #5 h.n ;
: r. ( a-a ) dup @ undef cr #1 + ;
: stack sp @ $08 for dup ss r. drop #-1 + next drop ;
: return rp @ #8 for #1 + dup rs r. drop next drop ;
: ok show black screen text green return r r. blue r. r. white r. r. green r. r. drop stack keyboard ; reset ok 

\ Block 207 
\
\ : 2/s n ( shift right n bits )
\ : state -a ( address of state vector for current computer )
\ : reset ( set registers undefined, execute from ROM )
\ : un. ( display undefined register )
\ : h.n nn ( display n hex digits of number )
\ : undef n ( bit 20 set means undefined )
\ : r. ( display register )
\ : stack ( display stack, top at top )
\ : return ( display return stack, top at bottom )
\ : ok ( display registers, b a blue, pc ir white ) 

\ Block 208 
( Registers )
: r state ;
: b state #1 + ;
: ar state #2 + ;
: pc state #3 + ;
: ir state #4 + ;
: t state #5 + ;
: s state #6 + ;
: slot state #7 + ;
: ss #7 and #8 + state + ;
: rs #7 and #16 + state + ;
: rp state #24 + ;
: sp state #25 + ;
: mem $2000 block + ; #4 +load #2 +load
: s1 ir @ #8 2/s inst ;
: s2 ir @ #3 2/s inst ;
: s3 #0 slot ! ir @ #4 and drop if ret then pc @ mem @ ir ! #1 pc +!
: s0 ir @ #13 2/s inst ;
: step slot @ jump s0 s1 s2 s3
: steps for step next ; 

\ Block 209 
\ ( Name 26 registers in state vector )
\ : ar -a ( A register. Cannot be named a because Pentium macro takes precedence )
\ : s0-s3 ( execute instruction from slot 0-3 )
\ : step ( execute next instruction )
\ : steps n ( execute n instructions ) 

\ Block 210 
( Instructions )
: nul ;
: call pc @ +r
: jmp ir @ $01FF and pc ! ;
: jz t @ dup or
: jc drop if #3 slot ! ; then jmp ;
: jns t @ $00020000 and jc ;
: ret -r pc ! ;
: @b b @
: @x mem @ +t ;
: @+ ar @ #1 ar +! @x ;
: n pc @ #1 pc +! @x ;
: @a ar @ @x ;
: !b b @ #1 b +!
: !x -t swap mem ! ;
: !+ ar @ #1 ar +! !x ;
: !a ar @ !x ;
: inst ( n ) #1 slot +! $1F and jump jmp jmp call call jz jz jns jns @b @+ n @a !b !+ nul !a -x 2*x 2/x +* orx andx nul +x 
r@ a@ t@ s@ r! a!x nul t! 

\ Block 211 
\ ( Define action of each instruction )
\ : inst n ( jump vector for 32 instruction codes ) 

\ Block 212 
( Instructions )
: +r ( n ) r @ rp @ #1 + dup rp ! rs ! r ! ;
: -r ( -n ) r @ rp @ dup rs @ r ! #-1 + rp ! ;
: +t ( n ) t @ s @ sp @ #1 + dup sp ! ss ! s ! t ! ;
: -t ( -n ) t @ s @ t ! sp @ dup ss @ s ! #-1 + sp ! ;
: -x t @ $0003FFFF or t ! ;
: 2*x t @ 2* $0003FFFF and t ! ;
: 2/x t @ dup $00020000 and 2* or 2/ t ! ;
: +* t @ #1 ? if s @ + then 2/ t ! ;
: orx -t t @ or t ! ;
: andx -t t @ and t ! ;
: +x -t t @ + $0003FFFF and t ! ;
: r@ -r +t ;
: a@ ar @ +t ;
: t@ t @ +t ;
: s@ s @ +t ;
: r! -t +r ;
: a!x -t ar ! ;
: t! -t drop ; 

\ Block 213 
\
\ : +r n ( push onto return stack )
\ : -r -n ( pop from return stack )
\ : +t n ( push onto data stack )
\ : -t -n ( pop from data stack )
\ : -x ( some instructions named with terminal x to avoid Pentium conflict ) 

\ Block 214 
( x18 target compiler ) empt #2097556 MagentaV h #2097555 MagentaV ip #2 MagentaV slot macro
: 2*s ?lit $E0C1 2, 1, ; forth
: memory $2000 block ;
: org ( n ) memory + dup h ! ip ! #0 slot ! ;
: , ( n ) h @ ! #1 h +! ;
: s3
: s0 h @ ip ! #13 2*s , #1 slot ! ;
: s1 #8 2*s
: sn ip @ +! #1 slot +! ;
: s2 #3 2*s sn ;
: i, slot @ jump s0 s1 s2 s3
: 25x #174 load ; #8 +load #2 +load #4 +load n x18 call class 25x 

\ Block 215 
\ ( Prototype for target compilers )
\ : h ( address of next available word in target memory )
\ : ip ( address of current instruction word )
\ : slot ( next available instruction slot )
\ : 2*s n ( shift left n bits )
\ : memory -a ( host address for target memory )
\ : org n ( set current target memory location )
\ : , n ( compile word into target memory )
\ : s0-s3 ( assemble instruction into slot 0-3 )
\ : i, ( assemble instruction into next slot )
\ : 25x ( compile code for multicomputer ) 

\ Block 216 
( Instructions )
: nop $1E i, ;
: adr ( n-a ) slot @ #2 or drop if nop then i, ip @ ;
: call defer ( a ) #2 adr +! ;
: if ( -a ) #4 adr ;
: -if ( -a ) #6 adr ;
: then ( a ) h @ $01FF and swap +! ;
: @+ $08 i, ;
: @b $09 i, ;
: n defer #8 f@ execute $0A i, , ;
: @ $0B i, ;
: !+ $0C i, ;
: !b $0D i, ;
: ! $0F i, ;
: - $10 i, ;
: 2* $11 i, ;
: 2/ $12 i, ;
: +* $13 i, ;
: or $14 i, ;
: and $15 i, ;
: + $17 i, ; 

\ Block 217 
\ ( Words being redefined for the target computer. These Pentium words can no longer be executed. Although Pentium macros 
\ still take precedence during compilation, they will no longer be used. )
\ : adr n-a ( assembles instruction, but not in slot 2, where address goes. Instruction address left on stack )
\ : call ( deferred to class. Executed for target defined words )
\ : then a ( puts address in low 9 bits of previous instruction word )
\ : n ( executed for green short-numbers. All 18-bit target numbers are short. Executes white short-number to put interp 
\reted number on stack. Then assembles literal instruction with number in next location ) 

\ Block 218 
( Instructions )
: pop $18 i, ;
: a $19 i, ;
: dup $1A i, ;
: over $1B i, ;
: push $1C i, ;
: a! $1D i, ;
: drop $1F i, ;
: ; #4 ip +! ; 

\ Block 219 
\ ( More target instructions )
\ : ; ( since it will be executed, it does not conflict with the Pentium macro ) 

\ Block 220 
( 25x ROM ) $0180 org $00 dup - dup - dup - dup - dup - dup - dup - dup - dup push push push push push push push push push 
a! a nop 

\ Block 222 
( Target )
: defer ( -a ) pop ;
: execute ( a ) push ;
: class ( a ) last #1 + ! ;
: f! ( an ) sp + ! ;
: f@ ( n-a ) sp + @ ; #1445 MagentaV ?com #1369 MagentaV csho
: empty empt #0 class csho @ ?com @
: functions ( aa ) #4 f! #6 f! ;
: x18 ( a ) #4 f@ ?com ! #6 f@ csho ! #1 f@ functions ; 

\ Block 224 


\ Block 225 
\ 

\ Block 226 
( Realtek rtl8139b ) macro
: move ( sdn ) $C189 2, drop $00C78957 3, drop $00C68956 3, $A4F3 2, $5F5E 2, drop ; forth
: 1us #1
: us ( n ) #550 #3 / * for next ;
: r ( n-a ) $02000000 device $14 + pci + 2/ 2/ ;
: rom ( a-n ) r @ ;
: 3rom ( nnn ) #4 rom #0 rom dup #16 for 2/ next swap ;
: tx ( -b ) $2000 block #4 * ;
: rx ( -b ) tx #1536 + ; #1 MagentaV ds #42 MagentaV fr
: n ( -a ) ds @ $10 r + ;
: send ( an ) fr @ tx + swap dup fr +! move ;
: first ( an ) n @ $2000 and drop if ds dup @ #1 + #3 and swap ! #0 fr ! send ; then first ;
: last ( an ) send tx ds @ $20 r + ! fr @ #60 max n ! ;
: reset $10000000 $34 r ! #100 us ;
: init rx $30 r ! 1us reset $0C000000 $34 r ! 1us $8A $44 r ! #3 ds ! $FB dup $21 p! $A1 p! sti
: /int $FFFF0001 $3C r ! ;
: rcvd ( -b ) $38 r @ dup $00010000 / $1FFF and $FFFFFFF0 + $38 r ! $10 + $1FFF and rx #4 + + ; 

\ Block 227 
\
\ : move sdn ( move n bytes from source to destination. Register 1 is used, 6 and 7 are saved )
\ : us n ( delay n microseconds. Edit cpu clock rate )
\ : r n-a ( word address of register )
\ : rom a-n ( fetch 2 bytes of mac )
\ : 3rom nnn ( 3 byte-pairs of mac )
\ : tx -a ( transmit buffer. 1536 bytes. Fragments must be assembled for transmission )
\ : rx -b ( receive buffer. 8k + 1532 byte overrun )
\ : ds -a ( must cycle thru 4 tx descriptors )
\ : fr -a ( must accumulate fragments in tx buffer )
\ : n -a ( tx status/length. Writing starts transmission )
\ : send an ( fragment into transmit buffer )
\ : first an ( fragment. Wait till buffer empty )
\ : last an ( fragment. Start transmission )
\ : reset ( controller )
\ : init ( ialize controller. Set tx/rx address/on and mac/broadcast. Enable irq10 )
\ : rcvd -b ( received packet. Register 38 is 10 bytes before start of next packet. Register 3a is end of current packet 
\ ) 

\ Block 228 
( Display registers )
: reg ( a ) dup r @ h. space #2 h.n cr ;
: regs $48 #19 for dup reg #-4 + next drop ;
: ok show red screen text regs keyboard ; 

\ Block 229 
\
\ : reg a ( display register and address )
\ : regs ( display interesting registers )
\ : ok ( diagnostic display )
\ : 48 ( counter. Neat! )
\ : 44 ( rx configuration )
\ : 40 ( tx configuration )
\ : 3c ( interrupt )
\ : 38 ( rx count/address )
\ : 34 ( command )
\ : 30 ( rx 8k ring buffer )
\ : 2c-20 ( tx address )
\ : 1c-10 ( tx status )
\ : c-8 ( multicast id, unused )
\ : 4 ( mac 54 )
\ : 0 ( mac 3210 ) 

\ Block 230 
( Ethernet ) empty #124 load
: empty empt logo cli ; macro
: w $66 1, ;
: w@ $8B 2, ;
: w! w $0289 2, drop ;
: *byte $C486 2, ; forth #126 load #128 load
: n@ w w@ $FFFF and *byte ;
: 2! a! w! ;
: n! a! *byte w! ;
: n, *byte 2, ;
: string pop ;
: packet string #-1 dup dup 2, 2, 2, 3rom 2, 2, 2, #0 n,
: length ( n ) packet #12 + n! ;
: 3! swap over 2! #2 + swap over 2! #2 + 2! ;
: ethernet ( n ) length packet #14 first ;
: +ethernet ( -a ) rcvd #14 + ; #132 load #134 load #136 load #138 load $72 interrupt
: serve forth receive /int 8clear /forth i; init ok 

\ Block 231 
\
\ : empty ( redefined to disable interrupts )
\ : w ( 16-bit prefix )
\ : w@ b-n ( fetch 16-bits from byte address )
\ : w! nb ( store 16-bits )
\ : *byte n-n ( swap bytes 0 and 1 )
\ : n@ b-n ( fetch 16-bit network-ordered number )
\ : 2! nb ( store 16-bit number )
\ : n! nb ( store 16-bit number in network order )
\ : n, n ( compile 16-bit number in network order )
\ : string -b ( returns byte address )
\ : packet -b ( ethernet packet header )
\ : dest -b ( destination field in packet )
\ : src -b ( source field )
\ : length n ( store length into packet )
\ : 3! nnnb ( store 3-word MAC )
\ : ethernet n ( send header with type/length )
\ : @ethernet -b ( return payload address of received packet ) 

\ Block 232 
( ARP for a single correspondent )
: . ( n ) 1, ;
: message string $01 n, $0800 n, $06 . $04 . $01 n,
: me 3rom 2, 2, 2, ( IP ) #0 . #0 . #0 . #2 .
: to #0 #0 #0 2, 2, 2, ( IP ) #0 . #0 . #0 . #1 .
: sender #8 + ;
: target #18 + ;
: dir #6 + ;
: ip #6 + w@ ;
: ar ( n ) message dir n! $0806 ethernet message #28 last ;
: arp cli #-1 dup dup packet 3! #1 ar sti ;
: -arp ( b-b ) dup #-2 + n@ $0806 or drop if ; then pop drop
: me? dup target ip message sender ip or drop if ; then dup sender packet #6 move
: query? dup dir n@ #1 or drop if ; then sender message target #10 move #4 ar ; 

\ Block 233 
\ ( Set ip addresses with Edit. Normal order, net bytes first )
\ : . n ( compile byte. Resembles URL punctuation )
\ : message -b ( 28-byte string )
\ : me ( comment marking my mac/ip address )
\ : to ( comment marking correspondent )
\ : sender
\ : target
\ : dir -b ( fields in either ) message ( or received message )
\ : ip b-n ( fetch ip address )
\ : ar n ( send query 1, or reply 4 )
\ : arp ( broadcast query )
\ : -arp b-b ( return if not ARP. Otherwise process and skip out. )
\ : me? b ( return if broadcast not for me. Save sender only in packet )
\ : query? b ( if a request, reply ) 

\ Block 234 
( ipv6 )
: header string $01000060 , $00 n, $17 . #64 .
: to $00 , $00 , $00 , ( IP ) #0 . #0 . #0 . #2 .
: me $00 , $00 , $00 , ( IP ) #0 . #0 . #0 . #1 .
: length ( n ) header #4 + n! ;
: dest header #20 + ;
: src header #36 + ;
: ip ( n ) $86DD ethernet length header #40 send ;
: +ip ( b-b ) dup #-2 + n@ $86DD or drop if pop ; then #40 + ; 

\ Block 235 
\ ( Set ip addresses with Edit. Normal order, net bytes first )
\ : header -a ( 40-byte ipv6 header )
\ : length n ( store 2-byte length in header )
\ : dest -a ( 4-byte destination ip address )
\ : src -a ( source ip )
\ : ip n ( send ip header embedded in ethernet packet )
\ : +ip b-b ( skip out if not IP. Otherwise return payload address ) 

\ Block 236 
( UDP )
: b@ ( b-n ) w@ $FF and ;
: header string #0 n, #0 n, #8 n, #0 n, #0 n,
: length ( n ) #8 + header #4 + n! ;
: udp ( n ) dup #8 + ip length ;
: +udp ( b-b ) dup #-34 + b@ $17 or drop if pop ; then #8 + ; 

\ Block 237 
\
\ : b@ b-n ( fetch byte )
\ : header -a ( 8-byte udp header )
\ : length n ( store length in header )
\ : udp n ( send ip header for n-byte packet )
\ : +udp b-b ( skip out if not UDP. Otherwise return payload address ) 

\ Block 238 
( Blocks to/from server )
: payload ( n-bn ) header #8 + n! header #10 ;
: +put ( nn ) #1026 udp over payload send + block 2* 2* #1024 last ;
: it ( b ) dup #2 + swap n@ #300 + block 2* 2* #1024 move ;
: -got ( b-b ) dup #-4 + n@ #2 #8 + or drop if it pop ; then ;
: receive +ethernet -arp +ip +udp -got
: +get ( b ) n@ #300 +put ;
: ... ( interrupt-protect words that transmit )
: get ( n ) cli #2 udp payload last sti ;
: put ( n ) cli #0 +put sti ;
: archive #161 for i put #1000 us -next ; lblk @ edit 

\ Block 239 
\ ( Client can get or put blocks to server )
\ : payload n-bn ( 2 bytes were appended to UDP header for block number )
\ : +put nn ( send block number. Append block as last fragment. Packet length distinguishes two messages )
\ : it b ( move 1024 bytes from packet to offset block )
\ : -got b-b ( if a 2-byte message, return. Otherwise move block to archive - 300+ - and skip out )
\ : receive ( check and decode received packet. ) +test ( returns if true, ) -test ( returns if false. Otherwise they ) pop 
\ ( - skip-out - return from ) receive. ( Resulting stack need not be empty, since ) /forth ( will restore pre-interrupt 
\ stack. ) pop ( must be in a word called by ) receive, ( it cant be nested )
\ : +get b ( send requested block from archive )
\ : get n ( send block number to request. Interrupt disabled lest reply interfer )
\ : put n ( send block )
\ : archive ( send blocks 0-161 - 9 cylinders ) 

\ Block 240 
( ipv4 )
: header align string $4500 n, #0 n, #1 n, #0 n, $FF00 n, #0 n, #0 , #0 ,
: length ( n ) #20 + header #2 + n! ;
: +id header #4 + dup n@ #1 + swap n! ;
: checksum ;
: source header #12 + ;
: destination header #16 + ;
: ip ( n-n ) dup #20 + $0800 ethernet length +id checksum header #20 send ; 

\ Block 242 
( Howerds test block ) empty macro
: gtend $7E 1, here invert + 1, ;
: init $B803F0BA , $EEEE0055 , ; forth
: h $01E5 ; ( h last class macros forths )
: allot ( n- ) h +! ;
: mk2 here $10 + ; $40 allot
: mk $01E2 ;
: class $01E9 ;
: macros $01EA ;
: forths $01EB ;
: mk macros @ mk2 ! forths @ mk2 #1 + ! h @ mk2 #2 + ! ;
: mt mk2 @ macros ! mk2 #1 + @ forths ! mk2 #2 + @ h ! ;
: reload #0 push ;
: qkey #3 for i next ; #57 MagentaV ky
: key pause $64 p@ #1 and drop if $60 p@ dup $3A - drop -if ky ! ; then drop then key ;
: kk key ky @ #57 - drop if kk then ;
: pt $03F0 ; here $04 / $12345678 , ,
: conf cli init $00 pt p! pt $01 + p@ $01 pt p! pt $01 + p@ ; 

\ Block 243 
\
\ : kk ( shows key values . press esc to exit ) 

\ Block 244 
( IR remote ) empty macro
: 2/s ?lit $F8C1 2, 1, ;
: p@ $EC 1, ;
: p! $EE 1, drop ;
: 1@ $8A 2, ;
: 1! a! $0288 2, drop ; forth
: ba #10 /mod $011F a! p! $0118 + a! ;
: b@ ba #0 p@ ;
: b! ba p! ;
: us #748 * time + -
: till dup time + drop -if till ; then drop ;
: ms #1000 * us ;
: array pop #2 2/s ;
: nul ; #3 MagentaV onf #145 load #146 load #50 load #147 load #148 load #149 load #150 load #151 load #152 load #153 load 
#155 load #154 load
: h keypd nul nul quit bye +db -db mute nul +xx -ch jp vcr tv0 dvd cd fm nul nul nul nul nul nul nul nul nul nul nul nul 
$00152500 , $00091016 , $11001016 , $0E0A1002 , #0 , #0 , #0 , 

\ Block 245 
\ ( smsc ircc2.0 IR Consumer mode ) $32 #10 b! #0 #12 b! #0 #20 b!
\ : buffer #200 block #4 * ;
\ : reset $10 #7 b! $80 #4 b! ;
\ : on $40 #5 b! ;
\ : off #2 #4 b! #200 ms ;
\ : emit #6 b@ $40 and drop if emit ; then #0 b! ;
\ : rdy #6 b@ $80 and drop ;
\ : get #0 b@ over 1! #1 + ;
\ : bytes for
\ : byte rdy if get dup buffer #4096 + or drop if byte ; then drop pop drop ; then next drop ;
\ : r #200 #1 wipes $80 dup #4 b! #5 b! buffer #1000000 bytes #0 #5 b! ;
\ : word - #4 for dup emit #8 2/s next drop ;
\ : cmd for word next #1
\ : sp for #0 word next ;
\ : rate #22 b! #21 b! ;
\ : sync $80 #20 b! ; 

\ Block 246 
( App: Slime : simple game ) empty ( sounds ) #4 +load 
macro
: @w $8B66 3, ; forth #2 MagentaV speed #13631840 MagentaV alice #29360784 MagentaV bob #0 MagentaV once #-1048576 MagentaV da 
#-16 MagentaV db #17 MagentaV delay #25 MagentaV /del #-1 MagentaV off #0 MagentaV done
: mova da @ alice +! ;
: movb db @ bob +! ;
: qpel ( a- ) @ $00010000 /mod at vframe xy @ $00010000 /mod swap $0400 * + $02 * + @w $FFFF and #0 + if #1 done ! #1 off 
! white bomb then ;
: clr #13 #65536 * #16 * #320 + alice ! #28 #65536 * #16 * #688 + bob ! #16 da ! #-16 db ! #0 delay ! #1 off ! #0 done ! 
#1 #1000 tn
: bgnd silver screen #16 #16 at black #1008 #672 box
: draw $FFFF color alice mova qpel #132 emit red bob movb qpel #133 emit ;
: tick off @ #0 + drop if ; then delay @ #-1 + delay ! -if /del @ delay ! draw click then ;
: b. ( c- ) $30 + 2emit ;
: ok show silver once @ #0 + drop if clr #0 once ! then silver #0 #708 at #600 #768 box #48 #708 at $00FFFF00 color #135 
mute @ #0 + drop if #1 + then 2emit #0 emit speed @ #1 + b. tick keypad ; nload x ok h 

\ Block 247 
\ ( slime ) empt macro
\ : @w ( 16bit fetch )
\ : speed ( selected speed )
\ : alice ( 16:16 bit xy coordinate of left slug )
\ : bob ( 16:16 bit xy coordinate of right slug )
\ : once ( is set to initialise the game )
\ : mova ( move alice by the value in da )
\ : movb ( move bob by the value in db )
\ : delay ( counts the ticks for each move )
\ : /del ( the reset value for delay )
\ : qpel ( check for slime coloured pixel )
\ : clr ( set alice and bob to start positions )
\ : bgnd ( draw the background )
\ : draw ( the slugs )
\ : tick ( do this every screen update )
\ : ok ( the screen display ) 

\ Block 248 
( Slime keypad )
: +speed #1
: +/-s speed @ + #0 max #9 min speed ! #10 speed @ 
invert + dup * #7 + #2 / #2 invert + /del ! ;
: -speed #-1 +/-s ;
: down #16 #65536 * da ! ; : up #-16 #65536 * da ! ;
: r #16 da ! ; : l #-16 da ! ;
: d2 #16 #65536 * db ! ; : u2 #-16 #65536 * db ! ;
: r2 #16 db ! ; : l2 #-16 db ! ;
: nul ;
: go #0 off ! ; : stop #-1 off ! ;
: x #1 once ! ;
: t off @ #0 + drop if #0 off ! ; then #-1 off ! ;
: help #249 edit ;
: mutet mute @ invert mute ! ;
: h keypd nul quit t nul nul nul nul nul l2 u2 d2 r2 x nul stop go nul nul nul nul l up down r -speed help mutet +speed $742E 
, #0 , $13121110 , $31302078 , #0 , $13121110 , $2B6E682D , 

\ Block 249 
\ ( Slime keypad )
\ : ludr ( move Alice and Bob left up down up )
\ : x ( reset the game )
\ : 0 ( stop the game )
\ : 1 ( start the game )
\ : - ( decrease the speed )
\ : h ( to see this help screen )
\ : m ( mute the sound - on/off )
\ : + ( increase the speed )
\ : . ( quit )
\ : t ( toggle on/off )
\ : slime: ( two players control Alice and Bob. The first to hit any slime or the edges loses. )
\ : credits: ( Coded by Howerd Oakford from an idea by Alan Crawley and Paul Chapman )
\ : type slime ( to play again ) 

\ Block 250 
( Sounds ) #20 MagentaV tempo #-1 MagentaV mute #90 MagentaV period
: tn ( ft- ) tempo @ * swap #660 #50 */
: hz ( tf- ) push #1000 #1193 pop */
: osc ( tp- ) dup period ! split $42 p! $42 p!
: tone ( t- ) mute @ #0 + drop if drop ; then $4F $61 p! ms $4D $61 p! #20 ms ;
: click #1 #90 osc ;
: t #3 tn ;
: q #8 tn ;
: c #16 tn ;
: 2tone #75 q #50 q ;
: h1 #50 c #54 q #50 q #45 c #60 c ;
: h2 #40 c #45 q #50 q #50 c #45 c ;
: h3 #54 c #60 q #54 q #50 c #45 q #40 q #50 t #45 t #50 t #45 t #45 #12 tn #40 q #40 #32 tn ;
: hh
: handel h1 h2 h3 ;
: piano #55 #7 for dup q #3 #2 */ next drop ;
: cetk #6 c #10 c #8 c #4 c #6 #32 tn ;
: bomb mute @ #0 + drop if ; then $4F $61 p! #500 for #1000 i invert + split $42 p! $42 p! #1 ms next $4D $61 p! #1 #32 tn 
; 

\ Block 251 
\ ( Sounds )
\ : tempo ( in ms per 1/8 quaver )
\ : mute ( equals -1 to disable sound )
\ : period ( test only - value sent to hardware )
\ : tn ( ft- play f Hz for t * 11 ms )
\ : hz ( tf- play t ms at f Hz )
\ : osc ( tp- play t ms of period p )
\ : tone ( t- play the current tone for t ms )
\ : click ( makes a click )
\ : t ( triplet )
\ : q ( quaver )
\ : c ( crotchet )
\ : 2tone ( 2 tones )
\ : h1
\ : h2
\ : h3
\ : hh
\ : handel ( part of Handels Gavotte )
\ : piano
\ : cetk ( Close Encounters of the Third Kind )
\ : bomb ( - well sort of .... ) 

\ Block 252 
( App: colorforth editor ) empty nload qinit
: eddd jblk @ ok h ( drop ) ;
: edd ( b- ) jblk @ jlast ! jblk ! eddd ; blk @ jblk ! #206 jlast ! eddd 

\ Block 253 
\ ( The colorforth editor in colorforth ) 

\ Block 254 
( Editor circular buffers ) #0 MagentaV cbn #0 MagentaV ends
: data ( - ) cbn @ $01 invert and cbn ! ;
: ptrs ( - ) cbn @ $01 or cbn ! ;
: heads ( - ) cbn @ $02 invert and cbn ! ;
: tails ( - ) cbn @ $02 or cbn ! ;
: cb@ ( -c ) ends @ cbn @ #8 * rshift $FF and ;
: cb! ( c- ) $FF and cbn @ #8 * lshift ends @ $FF cbn @ #8 * lshift invert and or ends ! ;
: cbnum ( -n ) cbn @ heads cb@ tails cb@ - $FF and swap cbn ! ;
: cbuf ( -a ) r@ $0100 / #2 + cbn @ $01 and + block ;
: tl- ( -n ) cbnum ?f drop 0if $00 ; then tails cb@ cbuf + @ cb@ #1 + cb! ;
: tl+ ( n- ) tails cbnum $FF - drop 0if tl- drop then cb@ $01 - cb! cb@ cbuf + ! ;
: hd@ ( -n ) heads cb@ cbuf + @ ;
: hd- ( -n ) cbnum $00 - drop 0if $00 ; then hd@ cb@ #1 - cb! ;
: hd! ( n- ) heads cb@ cbuf + ! ;
: hd+ ( n- ) cbnum $FF - drop 0if tl- drop then heads cb@ $01 + cb! hd! ; #4 +load 

\ Block 255 
\
\ : cbn ( bit 0 selects one of two circular buffers. Bit 1 selects head or tail value )
\ : cb@
\ : cb! ( read/write a byte to one of the 4 in ends selected by cbn )
\ : ptrs ( selects the pointer buffer )
\ : data ( selects the data buffer )
\ : heads ( selects the head value )
\ : tails ( selects the tail value )
\ : cbnum ( gives the number of items in the currently selected buffer )
\ : cbuf ( returns the address of the start of both buffers - the next 2 blocks )
\ : tl+
\ : tl-
\ : hd+
\ : hd- ( add or subtract from the head or tail of the currently selected buffer )
\ : ... ( note the tl- in hd+ . if the buffer is full we remove the oldest from the tail ) 

\ Block 256 
( r App:ay buffer string Undo Display r    i r    s r    t r    l r    f r    d r    0 r    o r    ; r   r r   rr r   rt 
r    e r   re r   ra r   rn r   ri r    a r   rc r   rl r   rf r   rd r    n r   r8 r   r; r   t r   tr r    i r   to r   te 
r   ta r   tn r    s r   ts r   tc r   tl r   tf r    c r   t0 r   t8 r   t; r   o r    l r   ot r   oo r   oe r   oa r    f 
r   oi r   os r   oc r   ol r    d r   od r   o0 r   o8 r   o; r    0 r   er r   et r   eo r   ee r    8 r   en r   ei r   es 
r   ec r    ; r   ef r   ed r   e0 r   e8 r   r r   a r   ar r   at r   ao r   rr r   aa r   an r   ai r   as r   rt r   al 
r   af r   ad r   a0 r   ro r   a; r   n r   nr r   nt r   re r   ne r   na r   nn r   ni r   ra r   nc r   nl r   nf r   nd 
r   rn r   n8 r   n; r   i r   ir r   ri r   io r   ie r   ia r   in r   rs r   is r   ic r   il r   if r   rc r   i0 r   i8 
r   i; r   s r   rl r   se r   sn r   ss r   sl r   rf r   s8 r   m r   mt r   me r   rd r   ms r   ml r   md r   m8 r   r0 
r   ct r   ce r   cn r   cs r   r8 r   cd r   c8 r   y r   yt r   r; r   yn r   ys r   yl r   yd r   t r   l r   lt r   le 
r   ln r   tr r   ll r   ld r   l8 r   g r   tt r   ge r   gn r   gs r   gl r   to r   g8 r   f r   ft r   fe r   te r   fs 
r   fl r   fd r   f8 r   ta r   wt r   we r   wn r   ws r   tn r   wd r   w8 r   d r   ds r   ti r   vs r   p r   ps r   b 
r   ts r   h r   hs r   x r   xs r   tc r   us r   q r   qs r   0 r   tl r   1 r   1s r   2 r   2s r   tf r   3s r   4 r   4s 
r   5 r   td r   6 r   6s r   7 r   7s r   t0 r   8s r   9 r   9s r   j r   t8 r   - r   -s r   k r   ks r   t; r   .s r   z 
r   zs r   ; r   : r   !s cccc cccc r   !s r   + r   +s r   @ bbbb r   @ r   ot bbbb r   *s ) 

\ Block 257 
\ 

\ Block 258 
( Display Undo string buffer ) #0 MagentaV jcur #64 MagentaV jblk
: sze ( -n ) $E0 ;
: qinit #0 ends ! $00 ptrs hd! $10000009 data hd! ;
: qnew ( - ) #0 ptrs hd+ ;
: qnum ( -c ) ptrs hd@ ;
: qpop ( -n ) data hd- ptrs hd@ #1 - if hd! ; then hd- drop drop ;
: qpush ( n- ) data hd+ ptrs hd@ $FF - drop 0if drop then ptrs hd@ #1 + hd! #0 #0 MagentaV pos #0 MagentaV lpos
: 2toc ( n-a ) jblk @ block pos @ + + ;
: xtoc? ( -n ) #1 2toc @ $0F and ;
: rtocs ( - ) jcur @ pos !
: ntocs ( -n ) #0 2toc @ $0F and #12 - ?f drop 0if #2 ; then #1 xtoc? ?f drop 0if #1 + then $FF and ;
: ltocs ( -n ) #0 pos !
: ltcs pos @ jcur @ - drop -if pos @ lpos ! ntocs pos +! ltcs drop then jcur @ lpos @ - ;
: mx ( n- ) jcur @ + #0 max #255 min jcur ! ;
: ml ltocs negate mx ;
: mu #8 for ml next ;
: mr rtocs mx ;
: md #8 for mr next ; nload 

\ Block 259 
\
\ : qinit ( initialises the queue pointers )
\ : qnew ( starts a new string entry )
\ : qnum ( -c number of cells in the top string )
\ : qpop ( -n returns the top cell of the top string )
\ : qpush ( n- stores n in the top string )
\ : ntocs ( number of tokens in the top string )
\ : qq ( n- ) qnew for qnum @ $0100 * $10000009 + qpush next cbnum drop ;
\ : qqq qinit #50 for #5 qq next #3 qq ;
\ : vvv ptrs cbnum data cbnum ;
\ : kk c vvv qpop ptrs hd@ ;
\ : gg cbuf dump ; 

\ Block 260 
( Editor Display ) #0 MagentaV cblind
: cb cblind @ #0 + drop ; #16 MagentaV state $10 MagentaV state*
: yellow $00FFFF00 color ;
: +txt white $6D emit space ;
: -txt white $6E emit space ;
: +imm yellow $58 emit space ;
: -imm yellow $59 emit space ;
: +mvar yellow $09 emit $11 emit $05 emit $01 emit space ;
: txts string $03010100 , $07060504 , $09090901 , $0F0E0D0C , ( ; )
: tx ( c-c ) $0F and txts + 1@ $0F and ;
: .new state @ $0F and jump nul +imm nul nul nul nul nul nul nul +txt nul nul +mvar nul nul nul ;
: .old state* @ $0F and jump nul -imm nul nul nul nul nul nul nul -txt nul nul nul nul nul nul ;
: state! ( n-* ) dup #0 + drop 0if drop ; then tx cb 0if drop ; then state @ swap dup state ! - drop if .old .new state @ 
#0 + if dup state* ! then drop then ; nload 

\ Block 261 
\
\ : state
\ : state! ( acts on a change of token type. It ignores extension tokens ) 

\ Block 262 
( Editor Display ) macro
: @b $8A 2, ; forth #160 MagentaV jcnt #206 MagentaV jlast #2 MagentaV jcol
: bksp xy @ #22 $00010000 * negate + xy ! ;
: ?.cur jcnt @ #1 + #255 min jcnt ! jcur @ jcnt @ negate + #1 + drop 0if $00FF4040 color bksp $30 emit white then ;
: x xy @ $00010000 / ;
: ?cr x #1000 negate + drop -if ; then
: ncr xy @ #30 + $FFFF and $00030000 xor xy ! ;
: emt ?cr emit ;
: emit emt ;
: emitw unpack if emit emitw ; then space drop drop ;
: emitcs unpack if #48 + emit emitcs ; then space drop drop ;
: dig pop + @b $FF and emit ;
: edig dig $1B1A1918 , $1F1E1D1C , $13052120 , $0E04100A ,
: odig dup $0F and swap 2/ 2/ 2/ 2/ $0FFFFFFF and ; nload 

\ Block 263 
\
\ : ncr ( new cr -does not get confused with original ) 

\ Block 264 
( CAPITALS HPO 2004 Editor Display )
: .hex odig if .hex edig ; then drop edig ;
: .dec #-1 ? -if negate #35 emit then
: n #10 /mod #-1 ? if .dec edig ; then drop edig ;
: num if $00C0C000 and color cb if #24 emit #21 emit then .hex space ; then color .dec space ;
: txt $00FFFFFF color emitw ;
: blu $FF color emitw ;
: cap $00FFFFFF color unpack #48 + emit emitw ; $00 MagentaV caps?
: caps $00FFFFFF color emitcs #-1 caps? ! ;
: ex bksp caps? @ ?f drop if caps ; then emitw ;
: gw $FF00 color emitw ;
: cw $FFFF color emitw ;
: yw $00FFFF00 color emitw ;
: coly #2 jcol ! ;
: colr #4 jcol ! ;
: colg #5 jcol ! ;
: colm #13 jcol ! ;
: colc #8 jcol ! ;
: colb #14 jcol ! ;
: rot $8B045E8B , $046E892E , $C38B0689 , $C3 1, #1220107268 MagentaV last nload 

\ Block 265 
\
\ : caps
\ : caps? ( is true if the extension token is CAPITALS )
\ : txt? ( returns true if the last token was text )
\ : .hex
\ : .dec 

\ Block 266 
( Editor display )
: short push dup 2/ 2/ 2/ 2/ 2/ swap $10 and drop pop num ;
: ys $00FFFF00 short ;
: long push #1 u+ $10 and drop dup @ pop num ;
: yn $00FFFF00 long ;
: gs $FF00 short ;
: gn $FF00 long ;
: var $00FF00FF color emitw #0 gn ;
: x xy @ $00010000 / ;
: rcr x #0 xor drop if cr then ;
: rw xy @ $FFFCFFFD + drop if rcr then $00FF0000 color cb if #41 emit space then emitw ;
: nuld drop ;
: .word ( w- ) dup #-16 and swap $0F and if $00 caps? ! then dup state! jump ex yw yn rw gw gn gs cw ys txt cap caps var 
blu nuld nuld ( ; )
: t #0 jcnt ! jblk @ block text #3 lm #1024 rm #3 #3 at $10 state ! $10 state* !
: n dup @ #-1 ? if ?.cur .word #1 + n ; then drop drop $0F state! ; white #103 emit ;
: ok show $00200040 color screen t keypad ; nload 

\ Block 267 
\ ( CAPITALSALLTHEWAY! ) 

\ Block 268 
( Editor aaaa bbbb cccc dddd keypad insertion )
: ripple ( a- ) dup dup @ over #1 + @ rot ! swap #1 + ! ;
: toc ( -a ) jblk @ block jcur @ + ;
: toend ( -n ) sze jcur @ - #0 max sze min ;
: del toc @ qpush toc toend for dup ripple #1 + next #0 swap ! drop ;
: dels jcur @ ?f drop 0if ; then ml qnew rtocs for del next ;
: ins ( n- ) sze jcur @ - ?f drop -if ; then jblk @ block sze + toend for #1 - dup ripple next ! ;
: undo qpop ins ;
: undos qnum ?f 0if drop ; then for undo next mr ; #25 MagentaV ky
: key pause $64 p@ #1 and drop if $60 p@ dup $3A - drop -if ky ! ; then drop then key ;
: lst ( n- ) jblk ! ok key drop ; nload 

\ Block 269 
\ ( Editor main keypad )
\ : ripple ( a- swaps the values at a and a+1 )
\ : bpush
\ : bpop ( push and pop the edit stack TBD )
\ : del ( removes the cell at the current cursor )
\ : dels ( removes the extension cells and one non extension coll before the cursor )
\ : undo ( puts back one cell )
\ : undos ( puts back one word which may have extension cells ) 

\ Block 270 
( Editor keypad cursor )
: btog ( n-n ) dup #1 and drop if #1 invert and dup jblk ! ; then #1 xor dup jblk ! ;
: cbtog cblind @ invert cblind ! ;
: lastb ( n-n ) jlast @ dup jblk ! swap jlast ! ;
: blkld jblk @ $FFFFFFFE and #-32 + drop -if ; then jblk @ load ;
: -blk ( n-n ) #-2 + #18 max dup jblk ! ;
: +blk ( n-n ) #2 + #252 min dup jblk ! ;
: accep drop xx ;
: h keypd nul dels accep undos coly colr colg btog ml mu md mr -blk colm colc +blk colb nul nul nul cbtog nul nul lastb blkld 
nul nul nul $00072515 , $2D0D010B , $0110160C , $2B0A0923 , $023A3800 , $03000029 , $3C , 

\ Block 272 
( App: Conways Game of Life ) empty nload
: 1cell ( n-- ) #32 /mod adj adj over over at #16 u+ 
   #16 + box ;          : nocell ( n-- ) drop ;
: draw ( n-- ) dup old @ #1 and jump nocell 1cell
: allcls ( -- ) #1023 for i draw -next ;
: gen ( -- ) #1023 for i tick swap new ! -next #1023 for i new @ i old ! -next ;
: locn ( --n ) row @ #32 * col @ + ;
: cur ( -- ) locn dup old @ $FF * $00FF0000 + color 1cell ;
: back ( -- ) black screen $00303010 color #40 #40 at 
   #583 dup box ;
: g ( -- ) show back green allcls gen keypad ;
: s ( -- ) gen show back blue allcls cur keypad ;
: clear ( -- ) #1500 #8 wipes #16 row ! #16 col ! s ;
: t ( -- ) locn old dup @ #1 xor swap ! ;
: col! ( n-- ) col +! col @ #31 and col ! ;
: l1 ( -- ) #-1 col! ;  : r1 ( -- ) #1 col! ;
: row! ( n-- ) row +! row @ #31 and row ! ;
: up1 ( -- ) #-1 row! ;  : dn1 ( -- ) #1 row! ;
: h keypd nul nul quit nul nul nul nul nul l1 up1 dn1 r1 nul nul nul nul glide glid2 glid3 glid4 clear s g t nul nul nul 
rando $2E00 , #0 , $13121110 , #0 , $1C1B1A19 , $74677378 , $52000000 , clear glide g h 

\ Block 273 
\
\ : s ( stop )
\ : g ( go )
\ : t ( toggle the square )
\ : ludr ( left up down right ) 
\    ( press s to stop then draw a shape using ludr and t to toggle ) 
\    ( then press g to go or s to single step )
\ : 1234 ( create gliders which move to the four corners counting clockwise from the top left ) 
\ ( R loads random numbers ) 

\ Block 274 
( Conways Game of Life ) #16 MagentaV row #16 MagentaV col
: old ( n-a ) cells #1500 block + ;
: new ( n-a ) cells #1504 block + ;
: rando ( -- ) #0 old $03FF for rand over ! cell+ next drop ;
: pos swap #32 /mod swap ;
: val #32 * + swap over old @ #1 and + ;
: up pos swap #31 + #31 and val ;
: dn pos swap #1 + #31 and val ;
: lt pos #31 + #31 and swap val ;
: rt pos #1 + #31 and swap val ;
: nul ;                 : n2 #0 ;
: s2 dup old @ #1 and ;  : y2 #1 ;
: tick dup #0 up lt dn dn rt rt up up nip jump n2 n2 s2 y2 n2 n2 n2 n2 n2
: adj ( nn--nn ) swap #17 * #40 + ;
: st ( rc- ) col @ + swap row @ + #32 * + old #1 swap ! ;
: glide ( -- ) #0 $02 st #0 #1 st #0 #0 st #1 #0 st #2 #1 st ;
: glid2 #0 #0 st #0 #1 st #0 #2 st #1 #2 st #2 #1 st ;
: glid3 ( -- ) #0 #2 st #1 #2 st #2 #2 st #2 #1 st #1 #0 st ;
: glid4 ( -- ) #0 #0 st #1 #0 st #2 #0 st #2 #1 st #1 #2 st ; 

\ Block 276 
( Wave audio SB, 8 bit, mono, no DMA ) empt macro
: pb@ 0 $EC 1, ;
: pb! $EE 1, drop ;
: /8 $0008F8C1 3, ; forth
: +base $0220 + ; ( * )
: ?rd $0E +base a!
: *?rd pb@ $80 ? drop if ; then *?rd ;
: ?wr $0C +base a!
: *?wr pb@ $80 ? drop if *?wr then ;
: dsp@ ?rd $0A +base a! pb@ ;
: dsp! ?wr pb! ;
: ?init dsp@ $AA or drop if ?init ; then ;
: 0dsp #6 +base a! #1 pb! #30 for pb@ drop next #0 pb! ?init $D1 dsp! ; 0dsp
: *dac! $10 dsp! dup dsp! /8 ;
: dac! *dac! *dac! *dac! *dac! drop ;
: length #2 + dup #-1 + @ 2/ 2/ ;
: ?data dup @ $61746164 or drop if length + ?data ; then length ;
: sound #100 block #3 + ?data ; ( * )
: play for dup @ dac! #1 + next drop ; 

\ Block 277 
\
\ : pb@ ( -n get byte from port )
\ : pb! ( n- put byte to port )
\ : /8 ( n-n shift 8 bit right )
\ : +base ( n-n add base adress )
\ : ?rd ( wait for DSP read ready )
\ : ?wr ( wait for DSP write ready )
\ : dsp@ ( -n read DSP )
\ : dsp! ( n- write DSP )
\ : ?init ( wait until initialized )
\ : 0dsp ( reset 3 us DSP, turn on speaker )
\ : dac! ( n- write 4 byte to DAC )
\ : length ( a-an return length of record )
\ : ?data ( a-an search data record )
\ : sound ( -an return address and length of sound data )
\ : play ( an- play sound ) 

\ Block 278 
( App: colorforth Explorer ) empty #9 MagentaV strt
: ?sze ( a- ) dup #510 block - drop ;
: crs ( n- ) ?f if for cr next #0 then drop ;
: docrs cr strt @ negate #0 max crs ;
: up1 ( a-a ) ?sze +if ; then $0400 + dup @ $FFFFFFF0 and $5C58BC80 - drop 0if ; then up1 ;
: upn ( n-a ) #0 max #64 block up1 swap ?f if for up1 next ; then drop ;
: ln ( a- ) #4 for cell+ dup @ dup $0F and $01 - ?f drop 
   0if drop leave then if dotsf then next drop ;
: .line ( a- ) ?sze +if drop ; then cr dup ablk . ln ;
: lines ( -- ) strt @ #0 max upn #20 strt @ negate #0 max - ?f if for blue dup .line up1 next then drop ; 
: marker iconh #11 * #4 - ;
: qok show $4228 color screen #240 #0 at cblk block ln $00 color #0 marker at #1023 marker #30 + box #0 #0 at 
docrs lines keypad ; 
nload 

\ Block 279 
\ ( Scans the first cell of each block for App: ) 
\ ( and displays the first 4 words after App: )
\ : +
\ : - ( step through the applications )
\ : ? ( displays the applications first shadow block )
\ : o ( loads the application )
\ : . ( requires ) .word ( from the editor )
\ : up1 ( takes the address of the start block and steps through even blocks until it finds a token App: ) 

\ Block 280 
( explorer )
: go strt @ #9 + upn ablk noshow ld ;
: md strt @ #1 - #-9 max strt ! ;
: mu strt @ #1 + #512 #4 / min strt ! ;
: qed strt @ #9 + upn ablk dup blk ! edit ;
: qh keypd nul quit go nul qed nul nul qed nul nul nul nul md nul nul mu nul nul nul nul nul nul nul nul nul nul nul nul 
$002D2500 , $3F202065 , $00 , $2B20202D , $00 , $00 , $00 , qok qh 

\ Block 281 
\ ( Scans the first cell of each block for App: ) 
\ ( and displays the first 4 words after App: )
\ : +
\ : - ( step through the applications )
\ : ? ( displays the applications first shadow block )
\ : o ( loads the application )
\ : . ( requires ) .word ( from the editor )
\ : up1 ( takes the address of the start block and steps through even blocks until it finds a token App: ) 

\ Block 282 
( fix the font )
: glyph ( c--a ) #48 * font @ + ;
: fix ( from to -- ) swap glyph swap glyph $30 cmove ;
: fixa $27 $3A fix $5B $01 fix $5C $02 fix $5D $03 fix 
$20 $04 fix ;
: fixb $28 $5B fix $29 $5D fix ; 

\ Block 283 
\ 

\ Block 284 
( grey = #-29727166  )oken ( test ) ( grey = #19138560  ) ( all tokens )
: tok ( t-n ) $11110000 + ;
: loc #2000 block ;
: set $10 for i #1 - tok loc i + ! next #9 tok loc ! #0 loc #17 + ! loc dump ; set 

\ Block 288 
( App: Timer Interrupt ) empty 
( Interrupts ) #170 load 
#0 MagentaV ticks 
: !pit $34 $43 p! ( lo ) $A9 $40 p! ( hi ) $04 $40 p! ; !pit
: pic1! $21 p! ; : pic2! $A1 p! ;
: p@ p@ ; : p! p! ;
: ttb $20 p@ $21 p@ $A0 p@ $A1 p@ ;
: bpic cli ( init ) $11 dup $20 p! $A0 p! 
( irq ) $00 pic1! $08 pic2! ( master ) #4 pic1! ( slave ) #2 pic2! ( 8086 mode ) #1 dup pic1! pic2! ( mask irqs ) $8F pic2 ! $B8 pic1! ;
: npic cli ( init ) $11 dup $20 p! $A0 p! 
( irq ) $20 pic1! $28 pic2! ( master ) #4 pic1! ( slave ) #2 pic2! ( 8086 mode ) #1 dup pic1! pic2! ( mask irqs ) $8F pic2 ! $B8 pic1! ; 
npic ( Note: npic will break bochs ) 
$20 interrupt
: timer0 forth: #1 ticks +! clear ;forth i; 
( cli to disable interrupts , sti to enable ) 
sti
: test cli #0 ticks ! #1 secs sti #100 secs cli ;
: tm cli #0 ticks ! ; 
lblk @ edit 
( Type bye after loading in bochs !!!! ) 

\ Block 289 
\ ( Timer Interrupt )
\ : !pit ( sets up the programable interval timer to ) 
\    ( 1 khz for a 1 ms tick ) 
\    ( for a clock of 14.31818 / 12 or 1.19318167 Mhz ) 
\    ( +/- 400 Hz this is actually 0.99985 +/- 0.0004 ) 
\    ( ms or about 0.015 percent fast. )
\ : pic1! ( write an octet to interrupt controller 1 )
\ : pic2! ( write an octet to interrupt controller 2 )
\ : !pic ( sets up the PIC chips ) 
\ $20 interrupt ( is the timer interrupt )
\ : timer0 ( the Forth code to run every timer tick ) 
\    ( use ) sti ( to enable interrupts, ) cli ( to disable )
\ : test ( run a 100 second test to time the timer ) 
<-- Unknown Blue token = 908FB00E crr+  
\    ( interrupt with respect to the Real Time Clock. )
\ : tm ( measure cpu ms in timer ticks ) 

\ Block 290 
( App: Sounds ) jmk #20 MagentaV tempo #0 MagentaV mute #1807 MagentaV period
: tn ( ft- ) tempo @ * swap #660 #50 */
: hz ( tf- ) push #1000 #1193 pop */
: osc ( tp- ) dup period ! split $42 p! $42 p!
: tone ( t- ) mute @ #0 + drop if drop ; then $4F $61 p! ms $4D $61 p! #20 ms ;
: click #1 #90 osc ;
: t #3 tn ;
: q #8 tn ;
: c #16 tn ;
: 2tone #75 q #50 q ;
: h1 #50 c #54 q #50 q #45 c #60 c ;
: h2 #40 c #45 q #50 q #50 c #45 c ;
: h3 #54 c #60 q #54 q #50 c #45 q #40 q #50 t #45 t #50 t #45 t #45 #12 tn #40 q #40 #32 tn ;
: hh
: handel h1 h2 h3 ;
: piano #55 #7 for dup q #3 #2 */ next drop ;
: cetk #6 c #10 c #8 c #4 c #6 #32 tn ;
: bomb mute @ #0 + drop if ; then $4F $61 p! #500 for #1000 i - + split $42 p! $42 p! #1 ms next $4D $61 p! #1 #32 tn ; 2tone 
jmt 

\ Block 292 
( App: Test block : ) empty #-3 MagentaV strt #62976 MagentaV lstup
: sze #256 block ;
: crs ( n- ) ?f if for cr next ; then drop ;
: up1 ( a-a ) dup sze - drop +if ; then $0100 + dup @ $FFFFFFF0 and $5C58BC80 - drop 0if dup lstup ! ; then up1 then ;
: .line ( a- ) dup sze - drop +if drop ; then cr dup $0100 / . #4 for #1 + dup @ dotsf next drop ;
: upn ( n-a ) ?f if #0 swap for up1 next ; then #0 ;
: lines strt @ negate #0 max crs strt @ #0 max upn #16 for up1 blue dup .line next drop drop ;
: ok show $00444444 color screen #240 #0 at r@ $0100 / block #4 for #1 + dup @ dotsf next drop $00 color #0 #266 at #1023 
#296 box #0 #0 at lines keyboard ;
: go strt @ #9 + upn $0100 / ld xx ;
: md strt @ #1 - #-8 max strt ! ;
: mu strt @ #1 + #256 min strt ! ;
: ?? strt @ #9 + upn $0100 / #1 + lst xx ;
: h keypd nul nul accept nul go nul nul ?? nul nul nul nul md nul nul mu nul nul nul nul nul nul nul nul nul nul nul nul 
$00 , $2F000003 , $00 , $2B000023 , $00 , $00 , $00 , ok h 

\ Block 293 
\ ( saving and restoring the dictionary )
\ : . ( allows just-in-time compilation )
\ : . ( the code for ) 2tone ( only exists for as long as it is needed ) 

\ Block 294 
( App: Serial terminal ) empty #52 load #48 load 
#65 MagentaV char #0 MagentaV qchar #0 MagentaV pos
: - ( nn-n ) negate + ;
: 0eq ( n- ) ?f if #0 #0 + drop ; then #1 #0 + drop ;
: 0neq #0 + drop ;
: eq ( nn- ) - 0eq ;
: crr pos @ $1E + $FFFF and pos ! ;
: cls black screen #0 pos ! ;
: act qchar @ 0eq if ; then pos @ $00010000 /mod swap at blue char @ chc emit xy @ pos ! char @ #13 eq if crr then char @ 
#12 eq if cls then #0 qchar ! ;
: wait pause qchar @ 0neq if wait then ;
: ch ( c- ) rkey? if rkey $FF and char ! #-1 qchar ! then ;
: ok c cls act #0 pos ! show ch act $00 #650 at $00202020 color #1024 #768 box keyboard ; 

\ Block 295 
\ ( The next two blocks are a 256 character 8*8 pixel font )
\ : . ( display characters statically on the screen )
\ : . ( type ) ok ( then ) #65 ch #13 ch #66 ch 

\ Block 296 
( App: Mouse test ) empty vars dump mark
: kk vars dump hex $04 for ekey ;is ekey ekey ekey ekey ekey c next ;
: tt
: ps2 $D4 $60 pc! ;
: mm kstat ;
:  ;iso 

\ Block 297 
\ 

\ Block 298 


\ Block 299 
\ 

\ Block 301 
\ ( Help screen ) 
\ ( F1 ) show this help screen or the start shadow 
\ ( F2 ) toggle number base between decimal and hex 
\ ( F3 ) toggle seeb display of blue words ( - ) blue
\ ( F4 ) editor, toggle colorforth / colorblind mode 

\ Block 302 
( App: Floppy disk driver ) macro
: - $35 1, $FFFFFFFF , ;
: delay $E1E6 2, ;
: p@ a! dup $EC 1, delay ;
: p! a! $EE 1, delay drop ;
: 1@ $8A 2, ;
: 1! a! $0288 2, drop ; forth
: on $1C $03F2 p! ;
: off $00 $03F2 p! ;
: err -if off warm ; then drop ;
: msr $03F4 p@ $C0 and ;
: out $00100000 for msr $80 or drop if *next $00 - ; then $03F5 p! pop drop 0 ;
: in $00100000 for msr $C0 or drop if *next $01 - ; then $03F5 p@ pop drop 0 ;
: cmd for out err next ;
: conf $00 $70 $00 $13 $04 cmd ; $03 $A2 $03 $03 cmd ;
: sense $08 $01 cmd ; nload off 

\ Block 303 
\
\ : - ( ones complement, sets flags )
\ : delay ( dummy write, some hardware seems to need this )
\ : on - ( activate floppy )
\ : off - ( turn motor off, reset FDC )
\ : err n - ( warm start if SF set )
\ : msr - n ( get main status register )
\ : out n - ? ( write a byte to the FIFO, return error on timeout )
\ : in - n ? ( read a byte from the FIFO, return error on timeout )
\ : cmd x n - ( send n bytes to the FIFO )
\ : conf - ( some FDC commands, )
\ : spec -
\ : sense - ( see documentation for details ) 

\ Block 304 
( Floppy disk driver )
: clrfifo in -if drop ; then drop drop clrfifo ;
: clrintr sense in err $80 and drop if clrfifo ; then clrfifo clrintr ;
: wait sense in err $80 and drop if clrfifo wait ; then clrfifo ;
: cal $00 $07 $02 cmd wait ;
: reset /flop $03 $A2 $03 $03 cmd $00 $70 $00 $13 $04 cmd ;
: init on pause spec conf clrintr cal ;
: xfer for in err over 1! $01 + next drop ;
: rd init push $FF $1B $12 $02 $01 $00 pop $00 $E6 $09 cmd block $04 * $0400 $12 * xfer off ;
: readid $00 $4A $02 cmd $07 for in err next clrintr ;
: version $10 $01 cmd in err $90 or drop if $02 - ; then 0 ; 

\ Block 305 
\
\ : clrfifo - ( discard all remaining input from the FIFO )
\ : clrintr - ( clear all pending interrupts )
\ : wait - ( wait for interrupt )
\ : cal - ( calibrate: move head to track 0 )
\ : reset - ( put FDC back to original state )
\ : init - ( initialize controller )
\ : xfer a n - ( reads n bytes from the FIFO to byte address a )
\ : rd b c - ( reads cylinder c to block b )
\ : readid ( for debugging )
\ : version - ? ( tests if your FDC supports enhanced commands ) 

\ Block 314 
( EEEEEEE qkqq ) 

\ Block 332 


\ Block 333 
\ 

\ Block 337 
\ 

\ Block 344 


\ Block 416 


\ Block 428 


\ Block 432 
( Relative load blocks )
: ll ( -- ) blk @ load ;
: sect ( --asn ) blk @ block blk @ 2* #2 ;
: ss ( -- ) sect writes drop drop ;
: uu ( -- ) sect reads drop drop ; 
#78 MagentaV lblk
: ld ( n- ) dup lblk ! load ;
: vv ( -- ) lblk @ edit ;
: help ( -- ) lblk @ #1 + edit ; 
( Real Time Clock )
: rtc@ ( t-c ) $70 p! $71 p@ ;
: rtc! ( ct- ) $70 p! $71 p! ;
: hi ( -- ) #10 rtc@ $80 and drop 0if hi ; then ;
: lo ( -- ) #10 rtc@ $80 and drop if lo ; then ; 
( processor clock ) #-3999 MagentaV khz
: calkhz ( -- ) hi lo time hi lo time - #500 + #1000 / dup khz ! ;
: ms ( n- ) khz @ * time + begin pause dup time invert + drop -if drop ; then end drop ;
: secs ( n- ) for pause lo hi next ; macro
: swapb ( w-w ) $E086 2, ; forth
: split ( w--cc ) dup swapb $FF and swap $FF and ; 

\ Block 433 
\ 

\ Block 434 
( Mandelbrot Set )
: o 0 0 dep @ #1 max for vndp itr vdup vlen $F0000000 + drop -if *next drop drop hole @ ; then drop drop pop hue ;
: mh x @ swap scrnw for o wf+ inc @ u+ next nip ;
: mv y @ scrnh for mh inc @ negate + next drop ;
: +d #2 dep +! : -d #-1 dep +! dep @ #1 max dep !
: draw vframe frame ! mv data ;
: ok c show keyboard ;
: l inc @ scrnw #1 - #8 */ negate x +! draw ;
: u inc @ scrnh #1 - #8 */ y +! draw ;
: d inc @ scrnh #1 - #8 */ negate y +! draw ;
: r inc @ scrnw #1 - #8 */ x +! draw ;
: +z inc @ #3 max dup scrnw #1 - #8 */ x +! dup scrnh #1 - #8 */ negate y +! #3 #4 */ #3 max inc ! draw ;
: -z inc @ #10000000 min dup scrnw #1 - #8 */ negate x +! dup scrnh #1 - #8 */ y +! #4 #3 */ inc ! draw ;
: hh home draw ; : hh2 h0 draw ;
: h keypd nul nul accept nul -d nul nul +d l u d r -z hh hh2 +z nul nul nul nul nul nul nul nul nul nul nul nul $2500 , $2B000023 
, $0110160C , $2B181423 , #0 , #0 , #0 , 

\ Block 435 
\ ( More Mandelbrot ) 
\ ( ludr move the cursor left right up down ) 
\ ( - + top row change depth detail ) 
\ ( - + bottom row change zoom ) 
\ ( h centres the image to the home location ) 
\ ( 0 resets depth and zoom ) 

\ Block 459 
\ 

\ Block 491 
\ 

\ Block 500 
( App: Icons font editor ) empty 
#0 MagentaV ic 

<-- Unknown Blue token = AC80000E gc  
<-- Unknown Blue token = AC80000E gc  
: showall ( -- ) #0 #0 #448 at #256 for dup emit #1 + next 
   drop ic @ #42 /mod #24 * #448 + swap #16 * 
   2dup at #16 #24 v+ red box ; 

\ Block 501 
\ ( Draw big-bits icon )
\ : @w a-n ( fetch 16-bit word from byte address )
\ : !w na ( store same )
\ : *byte n-n ( swap bytes )
\ : ic -a ( current icon )
\ : cu -a ( cursor )
\ : sq ( draw small square )
\ : xy -a ( current screen position, set by ) at
\ : loc -a ( location of current icons bit-map )
\ : 0/1 n-n ( color square depending on bit 15 )
\ : row a-a ( draw row of icon )
\ : +at nn ( relative change to screen position )
\ : ikon ( draw big-bits icon )
\ : adj nn-nn ( magnify cursor position )
\ : cursor ( draw red box for cursor )
\ : ok ( background task to continually draw icon, icon number at bottom ) 

\ Block 503 
\ 

\ Block 504 
( App: Stack usage analyser ) 
: oneline ( a-- ) cr dup $6600 - $08 * $6800 + blue h.4 space yellow $20 type ;
: ok show blue page text #8 lm #1024 rm 
   stacks $10 for dup oneline $20 + next drop 
   keypad ; 
ok 

\ Block 506 
( App: Server tasks ) 
#0 MagentaV var1
: ttsv1 serv1 #1000 ms #1 var1 +! ; 
: ksv1 isrv1 ; 
ttsv1 

#0 MagentaV var2
: ttsv2 serv2 #2000 ms #1 var2 +! ;
: ksv2 isrv2 ; 
ttsv2 

: ttclr #0 var1 ! #0 var2 ! ; 
: ttstop isrv1 isrv2 ; 

lblk @ edit 

\ Block 507 
\ 

\ Block 508 
( 04 )   ( gets )  .   cr   #18   +   #16   #1   type   #512   
     drop   then   cr   0if   and   $1F   @   line  o   onelin   blue   then   ;   sq   green   if   ?   $8000  
: 0/1   ;  
: showal   @   font   *   */   #8   #24   #16   ( n--a )  
: tofont   and   $FF   @   ic  
: loc   ;   +at   #17   *   #16   #-17   drop   next   2*   0/1   for   #16   *byte   @w   dup  
: row   ;   sq   blue   then   ;   sq   green   if   ?   $8000  
: 0/1   ;   +   @   font   *   */   #8   #24   #16   ( n--a )  
: tofont   and   $FF   @   ic  
: loc   ;   +at   #0   #17   box   +   #16   swap   +   #16   /mod   $00010000   @   xy  
: sq   
  forth   ;   2,   $C486  
: *byte   ;   drop   3,   $00028966   a!  
: !w     ;   3,   $8B66   : @w     macro   
  

\ Block 509 
\ #2 MagentaV icr   ( numbe )   ( icon )   
\   ;   cmove   */   #8   #24   #16   swap   tofont   swap   tofont  
\ : fcopy   
\   ;  d   keypa  l   showal   h.n   #2   emit   $78   emit   $30   .   green   dup   space   emit   dup   @   ic      ef 
\   white   at   #400   #400   box   #424   #416      ef   at   #400   #400   blue   text   ikon   at   dup   #18  r   curso 
\   page   show  
\ : ok   ;   box   +   #52   u+   #52   red   at   over   over   adj   adj   /mod   #16   @   cu  r  
\ : curso   ;   swap   *   #17  
\ : adj   ;   drop   next   +   #2   row   for   #24   loc  
\ : ikon  eu  
\ : aoeuao   

\ Block 510 
( Sandbox )
: test your code here ; 

\ Block 511 
\ ( Help screen ) 
\ ( F1 ) show this help screen or the start shadow 
\ ( F2 ) toggle number base between decimal and hex 
\ ( F3 ) toggle seeb display of blue words ( - ) blue
\ ( F4 ) editor, toggle colorforth / colorblind mode 
\ ( F5 ) rsn... 
\ ( F6 ) shows the last block edited 

\ Done.
