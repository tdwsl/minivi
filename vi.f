\ text editor

1 arg
create argument
dup c,
tuck here swap move allot

create lines 4000 cells allot
0 value nlines
128 1024 * constant line-buf-sz
create line-buf line-buf-sz allot
line-buf-sz line-buf + constant line-buf-end
0 value next-line
0 value x
0 value y
0 value dx
2variable xy1
create buf 256 allot
0 value vs
23 constant height
create lastbuf 2048 allot
0 value lastbuf-len
2variable last-lastbuf
2variable filename
0 0 filename 2!
0 value how-many
1 value last-many
0 value msg-len
60 value msg-max
0 value inserting?
2variable arg
2variable cmd
0 value changed?
create filename-buf 256 allot
create cbuf 2048 allot
0 value cnlines
0 value cbufp
create query 160 allot
0 value query-len

: line cells lines + ;

: len ( str -- str len ) 0 begin 2dup + c@ while 1+ repeat ;

: count-lines ( -- )
  1 to nlines
  line-buf lines !
  line-buf begin dup c@ while
    dup c@ 10 = if
      0 over c!
      1+ dup nlines line !
      nlines 1+ to nlines
    else 1+ then
  repeat drop nlines 1- to nlines ;

: blank-file ( -- )
  1 to nlines
  line-buf lines !
  0 line-buf c!
  line-buf 1+ to next-line ;

: load-file ( str len -- )
  r/o open-file throw >r
  line-buf line-buf-sz r@ read-file throw
  r> close-file throw
  line-buf + 0 over c!
  1+ to next-line
  count-lines
  0 to x 0 to y ;

: save-file ( str len -- )
  w/o open-file throw
  nlines 0 ?do i swap >r line @ len r@ write-line throw r> loop
  close-file throw ;

: print-lines ( -- )
  nlines 0 ?do
    i 1+ . i line @ len type cr
  loop ;

: move-line ( str len -- )
  >r next-line r@ move
  r> next-line + to next-line ;

: cap-line ( -- )
  0 next-line c!
  next-line 1+ to next-line ;

: compact-lines ( -- )
  line-buf to next-line
  nlines 0 ?do
    next-line
    i line @ move-line cap-line
    i line !
  loop ;

: move-line ( str len -- )
  dup next-line + line-buf-end >= if
    compact-lines dup next-line + line-buf-end >= throw
  then move-line ;

: left ( -- str len ) y line @ x ;

: right ( -- str len ) y line @ len x - >r x + r> ;

: insert ( str len -- )
  next-line >r
  left move-line
  move-line
  right move-line
  r> y line !
  cap-line ;

: insert-newline ( y -- )
  >r
  r@ line dup cell+ nlines r@ - cells move
  nlines 1+ to nlines
  next-line r> line ! ;

: insert-blankline ( y -- )
  insert-newline cap-line ;

: split-line ( -- )
  y 1+ insert-newline
  right move-line cap-line
  0 left + c! ;

: type-right ( -- )
  right type space
  right 1+ 0 ?do 8 emit loop drop ;

: ?line ( -- str len ) dup nlines < if line @ len else drop s" ~" then ;

: backspaces 0 ?do 8 emit loop ;

: .info ( -- )
  msg-len if buf msg-len msg-max min type 0 to msg-len
  else
    inserting? if [char] I else [char] C then emit space
    msg-max dup spaces backspaces
    filename @ if filename 2@ else s" <untitled>" then type
    [char] : emit y 1+ 0 .r [char] / emit nlines .
  then ;

: >message ( str len -- )
  dup to msg-len buf swap move ;

: >>message ( str len -- )
  dup >r msg-len buf + swap move r> msg-len + to msg-len ;

: <<message ( str len -- )
  buf 2dup + msg-len move dup msg-len + to msg-len
  buf swap move ;

: draw ( -- )
  y height - 1+ vs max y min nlines 1- min to vs
  page
  vs height + vs do
    i ?line type cr
  loop
  0 height at-xy .info
  0 0 at-xy
  y vs ?do
    i ?line type cr
  loop
  y line @ x type ;

' draw alias (draw)
defer draw
0 value drew?
: drew! 1 to drew? ;
: quiet ( -- ) ['] drew! is draw
  0 to drew? ;
: loud ( -- ) ['] (draw) is draw
  drew? if (draw) then ;
' (draw) is draw

: bind-x ( -- )
  y line @ len 1- dx min to x drop
  x 0 max to x ;

: bind-y ( -- )
  y 0 max nlines 1- min to y bind-x ;

: >lastbuf ( c -- )
  lastbuf lastbuf-len + c!
  lastbuf-len 1+ to lastbuf-len ;

: lastbuf- ( -- )
  lastbuf-len 1- 0 min to lastbuf-len ;

: key key dup 13 = if drop 10 then ;

: log-key ( -- c )
  key dup 127 = if lastbuf- else dup >lastbuf then ;

: unlog-key ( -- c )
  lastbuf-len dup 1+ to lastbuf-len
  lastbuf + c@ ;

defer *key
' log-key is *key

: .bs 8 emit space 8 emit ;

: accept-insert ( -- )
  1 to inserting? draw
  0 begin
    *key dup 27 = if drop
      dup buf swap insert x + 1- to dx bind-x
      0 to inserting? draw exit then
    dup 10 = if drop
      split-line
      buf swap insert
      0 to x y 1+ to y
      draw 0
    else dup 127 = if drop
      dup if 1- .bs type-right then
    else dup emit over buf + c! 1+ type-right then then
  again ;

: x-after ( -- )
  x 1+ dup y line @ len nip <= if to x
    y line @ x 1- + c@ emit
  else drop then ;

: x-eol ( -- )
  right 1- type
  y line @ len to dx drop bind-x ;

: x-start ( -- )
  y line @
  0 to x
  13 emit
  len 0 ?do
    dup i + c@ 32 <= if 32 emit i 1+ to x else leave then
  loop drop
  x to dx ;

: del ( -- )
  y line @ dup c@ 0= if drop exit then
  x + dup 1+ over len move
  y line @ x + c@ 0= x and if 8 emit x 1- to dx bind-x then type-right ;

: del-line ( y -- )
  dup >r line dup cell+ swap nlines r> - cells move
  nlines 1- to nlines ;

: walk-back ( str len -- str len )
  begin dup while 2dup + 1- c@ 32 <= while 1- repeat then ;

: join-line ( -- )
  y 1+ nlines = if x y line @ len nip < if draw then exit then
  y line @ len walk-back dup >r +
  y 1+ to y
  x-start y line @ x + len dup if 1+ >r swap 32 over c! 1+ r> move
  else drop 2drop then
  y del-line
  y 1- to y r> 1+ to dx bind-x draw ;

: cut-line ( -- )
  y line @ len 1+ >r cbufp r@ move
  r> cbufp + to cbufp
  y del-line ;

: cut-lines ( n -- )
  dup y + nlines > if drop exit then
  cbuf to cbufp
  to cnlines
  y dup cnlines + to y
  cnlines 0 do y 1- to y cut-line loop
  to y bind-y draw ;

: uncut-lines ( -- )
  cnlines 0= if s" empty buffer" >message draw exit then
  cbuf cnlines 0 do
    y insert-newline next-line y line !
    len 1+ 2dup move-line +
  loop drop draw ;

: command ( c -- )
  x y xy1 2!
  dup case
  [char] i of accept-insert endof
  [char] a of x-after accept-insert endof
  [char] A of x-eol x-after accept-insert endof
  [char] I of x-start accept-insert endof
  [char] o of y 1+ dup to y insert-blankline bind-x accept-insert endof
  [char] O of y insert-blankline bind-x accept-insert endof
  [char] x of del endof
  [char] X of x 0> if x 1- to dx bind-x 8 emit del then endof
  [char] J of join-line endof
  [char] d of
    how-many *key case
    [char] d of cut-lines endof
    [char] j of 2* cut-lines endof
    [char] k of y over - dup 0>= if to y 1+ cut-lines else 2drop then endof
    drop endcase
    1 to last-many
  endof
  [char] P of uncut-lines endof
  [char] p of y 1+ to y uncut-lines endof
  dup of drop last-lastbuf 2@ to lastbuf-len lastbuf c! exit endof
  endcase
  lastbuf-len last-lastbuf 2!
  1 to changed? ;

: repeat-last ( n -- )
  ['] unlog-key is *key
  quiet
  0 do
    0 to lastbuf-len
    *key command
  loop
  loud
  ['] log-key is *key ;

: go-right ( -- )
  how-many 0 do
    x 1+ y line @ len nip < if
      y line @ x + c@ emit
      x 1+ to dx bind-x then
  loop ;

: go-left ( -- )
  how-many 0 do x if x 1- to dx bind-x 8 emit then loop ;

: go-down ( -- )
  y how-many + nlines 1- min to y bind-x draw ;

: go-up ( -- )
  y how-many - 0 max to y bind-x draw ;

: trim-head ( str len -- str len )
  begin dup while over c@ 32 <= while 1- >r 1+ r> repeat then ;

: trim-tail ( str len -- str len )
  begin dup while 2dup 1- + c@ 32 <= while 1- repeat then ;

: strchr ( str len c -- str len )
  >r begin dup while over c@ r@ <> while 1- >r 1+ r> repeat then r> drop ;

: split-cmd ( str len -- str len str len )
  trim-head 2dup 32 strchr trim-tail
  >r dup >r nip over - r> r> trim-head ;

: ?filename ( -- str len -1 | 0 )
  arg @ if arg 2@ -1 exit then
  filename @ if filename 2@ -1 exit then
  s" no filename" >message 0 ;

: filename! ( str len -- )
  >r filename-buf r@ move filename-buf r> filename 2! ;

: cmd-load ( -- )
  ?filename if
    >message buf msg-len
    ['] load-file catch if
      s" failed to load " <<message
    else buf msg-len filename! s" loaded " <<message 0 to changed? then
  then ;

: cmd-save ( -- )
  ?filename if
    >message buf msg-len
    ['] save-file catch if
      s" failed to save " <<message
    else buf msg-len filename! s" saved " <<message 0 to changed? then
  then ;

: cmd-saveq ( -- )
  ?filename if
    2drop cmd-save buf c@ [char] f <> if page bye then
  then ;

: enter-command ( buf c -- )
  0 height at-xy msg-max dup spaces backspaces emit
  >r 0 begin key dup 10 <> while
    dup 127 = if .bs drop
      ?dup 0= if r> drop -1 exit then 1-
    else dup emit over r@ + c! 1+ then
  repeat r> 2drop ;

: colon-command ( -- )
  buf [char] : enter-command dup 0<= if drop draw exit then
  buf swap split-cmd arg 2! cmd 2!
  cmd 2@ s" q" str= if
    changed? if s" unsaved changes, use q! to quit" >message draw exit then
    page bye then
  cmd 2@ s" q!" str= if page bye then
  cmd 2@ s" w" str= if cmd-save draw exit then
  cmd 2@ s" wq" str= cmd 2@ s" x" str= or if cmd-saveq draw exit then
  cmd 2@ s" o" str= if cmd-load draw exit then
  cmd 2@ >message s" unknown command " <<message draw ;

: yx+ ( y x -- y x )
  over line @ over + c@ if 1+
  else drop 1+ dup nlines = if drop 0 s" wrapped around" >message then 0 then ;

: search ( -- )
  query-len 0= if s" no previous search" >message draw exit then
  y x begin yx+
    over line @ over + query query-len ( s1 s2 len )
    begin dup while >r over c@ over c@ = r> swap while
    1- >r 1+ >r 1+ r> r> repeat then nip nip
    0= if to x to y draw exit then
  over y = over x = and until
  query query-len >message s"  not found" >>message draw ;

: enter-search ( -- )
  query [char] / enter-command ?dup 0= if search exit then
  dup -1 = if drop draw exit then
  to query-len search ;

: pgdn vs height 1- + dup to vs to y bind-y draw ;
: pgup vs if vs height 1- - 0 max dup to vs height 1- + to y bind-y then draw ;

: control ( c -- )
  dup case
  [char] h of go-left endof
  127      of go-left endof
  [char] j of go-down endof
  10       of go-down endof
  [char] k of go-up endof
  [char] l of go-right endof
  32       of go-right endof
  [char] . of last-many repeat-last exit endof
  2        of pgup endof
  6        of pgdn endof
  [char] 0 of x to how-many go-left endof
  [char] : of colon-command endof
  [char] / of enter-search endof
  [char] n of search endof
  dup of 0 endof endcase if exit then
  dup lastbuf c!
  1 to lastbuf-len
  how-many to last-many
  command
  last-many 1- ?dup if repeat-last then ;

: key,times ( -- c n )
  key dup [char] 0 = if 1 exit then
  0 >r begin dup [char] 0 >= over [char] 9 <= and while
    [char] 0 - r> 10 * + >r key repeat
  r> ?dup 0= if 1 else dup to last-many then ;

: main ( -- )
  argument count dup if 2dup filename 2! ['] load-file catch if blank-file then
  else 2drop blank-file 0 0 filename 2! then
  draw begin
    key,times to how-many
    control
  again ;

main

