dim dynamic dta%(22)
call cari("c:\windows\fonts\*.ttf"+chr$(0),dta%(0),ern%)
call getnamafiledta(dta%(), nama$)
pathnama$ = "c:\windows\fonts\" + nama$
call bkfil(pathnama$+chr$(0), 0, f1%)
 call ptrmove(f1%,59263)
 yeah$ = space$(10)
 call bcstrf(f1%,yeah$)
 ? yeah$;";"
call tpfil(f1%)

end
sub getnamafiledta(dump%(0), nam$)
 tmp$ = string$(12,0)
 call namadta(dump%(0),tmp$)
 nam$ = fnunascz$(tmp$)
end sub
sub CARI inline '(mask$,dumpdta%[22],erno%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H56
 $inline &H0A, &HB4, &H1A, &HCD, &H21, &HC5, &H76, &H0E, &H8B, &H54
 $inline &H02, &HB9, &H17, &H00, &HB8, &H00, &H4E, &H8E, &HDB, &HCD
 $inline &H21, &HB4, &H00, &HC4, &H7E, &H06, &HAB, &H1F, &H5D
End Sub
sub CARILAGI inline '(dta%[22],erno%)
 $inline &H55, &H89, &HE5, &H1E, &HB4, &H1A, &HC5, &H56, &H0A, &HCD
 $inline &H21, &HB8, &H00, &H4F, &HCD, &H21, &H30, &HE4, &HC4, &H7E
 $inline &H06, &HAB, &H1F, &H5D
End Sub
sub BKFIL inline '(namafile$, akses, filehandler%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HB4, &H3D
 $inline &HC5, &H76, &H0A, &HAC, &HC5, &H76, &H0E, &H8B, &H54, &H02
 $inline &H8E, &HDB, &HCD, &H21, &H72, &H08, &HB4, &H00, &HC4, &H7E
 $inline &H06, &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB
 $inline &H1F, &H5D
End Sub
sub NAMADTA inline '(dta%[22],nama$[12])
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H7C, &H02, &HC5, &H76, &H0A, &H83, &HC6, &H1E, &H8E
 $inline &HC0, &HB9, &H0C, &H00, &HF3, &HA4, &H1F, &H5D
End Sub
sub STROASCZ inline '(string$,Lennya)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &H31, &HDB, &HC5
 $inline &H76, &H0A, &H8B, &H0C, &H8B, &H74, &H02, &H8E, &HD8, &HE3
 $inline &H09, &HAC, &H3C, &H00, &H74, &H04, &H43, &H49, &HEB, &HF5
 $inline &H89, &HD8, &HC4, &H7E, &H06, &HAB, &H1F, &H5D
End Sub
def fnunascz$(ascz$)
 call STROASCZ(ascz$,tru%)
 fnunascz$ = left$(ascz$,tru%)
end def
sub TPFIL inline '(handler%)
 $inline &H55, &H8B, &HEC, &H1E, &HC5, &H76, &H06, &H8B, &H1C, &HB4
 $inline &H3E, &HCD, &H21, &H1F, &H5D
End Sub
sub BCSTRF inline (handler,string$)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H0C, &H8B, &H54, &H02, &HC5, &H76, &H0A, &H8B, &H1C
 $inline &H8E, &HD8, &HB4, &H3F, &HCD, &H21, &H1F, &H5D
End Sub
sub PDPTR inline '(hand%,cx,dx,mode)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8B, &H1C, &HC5
 $inline &H76, &H0E, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H14, &HC5
 $inline &H76, &H06, &HAC, &HB4, &H42, &HCD, &H21, &H1F, &H5D
End Sub
sub ptrmove(lvfileno%, lvbanyak&)
 call lng2int(lvbanyak&,tmp%,tmp2%)
 call pdptr(lvfileno%,tmp%,tmp2%,1)
end sub
sub LNG2INT inline '(asf!,hi%,lo%)
 $inline &H55,&H89,&HE5,&H1E,&HC5,&H76,&HE,&H66,&H8B,&H14
 $inline &H89,&HD0,&HC4,&H7E,&H6,&HAB,&HB1,&H10,&H66,&HD3
 $inline &HEA,&H89,&HD0,&HC4,&H7E,&HA,&HAB,&H1F,&H5D
end sub