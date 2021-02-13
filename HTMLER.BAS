screen 0
def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
open "e:\coba.html" for output as #1
 print #1, "<select name=tanggal>"
 for tanggal% = 1 to 31
  print #1, " <option value=";fnkarakter$(tanggal%);">";fnkarakter$(tanggal%);"</option>"
 next tanggal%
 print #1, "</select>"

 print #1, "<select name=bulan>"
 for bulan% = 1 to 12
  print #1, " <option value=";fnkarakter$(bulan%);">";fnkarakter$(bulan%);"</option>"
 next bulan%
 print #1, "</select>"

 print #1, "<select name=tahun>"
 for tahun% = 1980 to 2004
  print #1, " <option value=";fnkarakter$(tahun%);">";fnkarakter$(tahun%);"</option>"
 next tahun%
 print #1, "</select>"

close #1
? "selesai"