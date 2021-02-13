screen 0
file$ = command$
if file$ = "" then file$ = "e:\webku\mp3info.htm"

open file$ for input as #1
' open "e:\webku\coba.htm" for output as #2
 ? "<script language=vbscript>"
 while not eof(1)
  line input #1, garis$
  posi% = instr(garis$,"<")
  if posi% <> 0 then
   garis2$ = mid$(garis$,posi%+1)
   garis1$ = left$(garis$,posi%)
   garis$ = chr$(34)+garis1$+chr$(34)+"+"+chr$(34)+garis2$+chr$(34)
  end if
  ? tab(8);"document.writeln(";garis$;")"
 wend
 ? "</script>"
' close #2
close #1
'cara supaya
