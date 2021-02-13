screen 0
randomize timer
def fnacak%(min%,max%)
 fnacak% = rnd*(max%-min%)+min%
end def
sub oneKEY inline '(codeno2,kode1)
 $inline &H55, &H89, &HE5, &H1E, &H31, &HC0, &HCD, &H16, &HC4, &H7E
 $inline &H06, &HAA, &HC4, &H7E, &H0A, &H88, &HE0, &HAA, &H1F, &H5D
End Sub
dim subjek$(50)
dim predikat$(50)
dim objek$(50)
dim ketsub$(50)

open "subjek.txt" for input as #1
 ctrsubjek% = 0
 while not eof(1)
  incr ctrsubjek%
  line input#1, masuk$
  subjek$(ctrsubjek%) = masuk$
 wend
close #1

open "predikat.txt" for input as #1
 ctrpredik% = 0
 while not eof(1)
  incr ctrpredik%
  line input#1, masuk$
  predikat$(ctrpredik%) = masuk$
 wend
close #1

open "objek.txt" for input as #1
 ctrobjek% = 0
 while not eof(1)
  incr ctrobjek%
  line input#1, masuk$
  objek$(ctrobjek%) = masuk$
 wend
close #1

open "ketera~1.txt" for input as #1
 ctrketsub% = 0
 while not eof(1)
  incr ctrketsub%
  line input#1, masuk$
  ketsub$(ctrketsub%) = masuk$
 wend
close #1

'pengambilan acak
do
 ? subjek$(fnacak%(1,ctrsubjek%));" ";
 ? ketsub$(fnacak%(1,ctrketsub%));" ";
 ? predikat$(fnacak%(1,ctrpredik%));" ";
 ? objek$(fnacak%(1,ctrobjek%))
 call onekey(kode2%,kode1%)
loop until kode1% = 27