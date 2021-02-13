screen 0
'this program convert several row of string into 5 word per row
'asfsaf
'asfdas
'adssa
' menjadi "asfsafas" "Asfdasf" "asfsafas"
sub onekey inline '(buttno, codeno) '(kodetombol%, ascii%)
 $inline &H55, &H89, &HE5, &H1E, &H31, &HC0, &HCD, &H16, &HC4, &H7E
 $inline &H06, &HAA, &HC4, &H7E, &H0A, &H88, &HE0, &HAA, &H1F, &H5D
End sub
def fnpathke$(berapa%, pathname$)
 local jmlkata%, temp$
 jmlkata% = fngeneration%(pathname$+" ")
 for katake% = 1 to jmlkata%-berapa%
  temp$ = temp$ + fnpath$(fnkatake$(katake%,pathname$,"\"))
 next katake%
 fnpathke$ = temp$
end def
def fnkatake$(kk,m$,p$)
 m$ = m$ + p$
 if kk = 1 then
  starthu = 1
  stophu  = fnjedake(1,m$,p$)-1
 else
  starthu = fnjedake(kk-1,m$,p$)+1
  stophu  = fnjedake(kk,m$,p$)-1
  call panstr(m$,pan%)
  if stophu <= -1 then stophu = pan%
 end if
 fnkatake$ = fnkalimat$(m$,starthu,stophu)
end def
def fnkalimat$(master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
def fnnamaext$ (any$)
 if instr(any$, "\") <> 0 then
  tmp% = len(any$)
  for a% = 0 to tmp%-1
   if mid$(any$, tmp%-a%,1) = "\" then exit for
  next a%
  fnnamaext$ = mid$(any$, tmp%-a%+1)
 else
  fnnamaext$ = "????????.???"
 end if
end def
def fnjedake(brp%,m$,p$)
local start%
 i.hrfke = brp%
 start% = 1
 do
  if i.hrfke <=0 then quitjedake
  start% = instr(start%,m$,p$) + 1
  i.hrfke = i.hrfke - 1
 loop
 quitjedake:
  fnjedake = start% - 1
end def
def fngeneration%(pathname$)
 local jmlkata%, temp%, temp$
 if pathname$ <> "" then
  do
   temp% = instr(temp%+1, pathname$,"\")
   temp$ = mid$(pathname$,temp%+1,1)
   if (temp$ <> "\") and (temp$ <> "") then jmlkata% = jmlkata% + 1
  loop until temp% = 0
 end if
 fngeneration% = jmlkata%
end def
def fnpath$ (any$)
 if instr(any$, ".") = 0 then
  path$ = any$
  if (right$(path$,1) <> "\") and (path$ <> "") then path$ = path$ + "\"
  fnpath$ = path$
  exit def
 end if
 For a% = len(any$) to 1 step - 1
  if mid$(any$, a%,1) = "\" then exit for
 Next a%
 if a% = 0 then
  path$ = any$
 else
  path$ = left$(any$, a%)
 end if
 if (right$(path$,1) <> "\") and (path$ <> "") then path$ = path$ + "\"
 fnpath$ = path$
end def

sum$ = command$
? "inputnya file : ";sum$
input "outputnya file: ";tuj$
if sum$ = "" or tuj$ = "" then
 ? "yg bener dong"
 call onekey(dum%,dum%)
 end
end if

open sum$ for input as #1
 open tuj$ for output as #2
  while not eof(1)
   line input #1, pathname$
   path$ = fnpathke$(1,pathname$)
   filename$ = fnnamaext$(pathname$)
   if path$ <> pathx$ then
    if not baru% then
     print #2,""
     baru% = 0
    end if
    print #2,path$;filename$
    print #2,space$(5);
    pathx$ = path$
    ctr% = 0
   else
    print #2, chr$(34);filename$;chr$(34);" ";
    ctr% = ctr% + 1
    if ctr% >4 then
     print #2, ""
     print #2,space$(5);
     ctr% =0
    end if
   end if
  wend
 close #2
close #1
? "Selesai"