sub INSTALfont inline '(memorihuruf%[setengahukuranfile], namafont$, erno%)
 $inline &H55, &H8B, &HEC, &H1E, &H8B, &H1E, &H00, &H00, &HB9, &H16
 $inline &H00, &H50, &H49, &H75, &HFC, &HB4, &H1A, &H16, &H1F, &H8B
 $inline &HD4, &HCD, &H21, &HB4, &H4E, &HB9, &H17, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H54, &H02, &H8E, &HDB, &HCD, &H21, &H72, &H02
 $inline &HEB, &H09, &HC4, &H7E, &H06, &HB8, &H02, &H00, &HAB, &HEB
 $inline &H1A, &H8B, &HF4, &H83, &HC6, &H1A, &H36, &H8B, &H0C, &HB8
 $inline &H00, &H3D, &HCD, &H21, &H8B, &HD8, &HB4, &H3F, &HC5, &H56
 $inline &H0E, &HCD, &H21, &HB4, &H3E, &HCD, &H21, &HB9, &H16, &H00
 $inline &H58, &H49, &H75, &HFC, &H1F, &H5D
End Sub
sub VHURUF inline '(fontmemori%[0], segmen%, x, y, teks$, warna)
 $inline &HE9, &H4D, &H00, &H57, &H36, &H8A, &H4E, &HFB, &H80, &HF9
 $inline &H00, &H74, &H08, &H81, &HC7, &H40, &H01, &HFE, &HC9, &H75
 $inline &HF3, &H1E, &HC5, &H76, &H06, &H8A, &H24, &H1F, &H36, &H8A
 $inline &H76, &HFA, &H36, &H8B, &H76, &HFC, &H80, &HFE, &H00, &H74
 $inline &H25, &H57, &HFE, &HCE, &HAC, &H8A, &HD8, &HB2, &H01, &HB1
 $inline &H08, &H20, &HD0, &H75, &H11, &H8A, &HC3, &H81, &HC7, &H40
 $inline &H01, &HD0, &HE2, &HFE, &HC9, &H75, &HF0, &H5F, &H47, &HE9
 $inline &HDC, &HFF, &H26, &H88, &H25, &HE9, &HE9, &HFF, &H5F, &HC3
 $inline &H55, &H8B, &HEC, &H1E, &H50, &H50, &H8B, &H1E, &H00, &H00
 $inline &HC5, &H76, &H0E, &HAD, &H8B, &HF8, &HD1, &HE7, &HD1, &HE7
 $inline &H01, &HC7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HC5, &H76, &H12, &H03, &H3C, &HC5
 $inline &H76, &H16, &H8E, &H04, &HC5, &H76, &H0A, &H8B, &H0C, &H8B
 $inline &H74, &H02, &H8E, &HDB, &HB7, &H00, &H83, &HF9, &H00, &H74
 $inline &H6C, &H49, &HAC, &H1E, &H56, &H51, &HC5, &H76, &H1A, &H8A
 $inline &H24, &H8A, &HDC, &H46, &HB7, &H00, &H38, &H04, &H74, &H13
 $inline &HFE, &HC7, &HFE, &HCB, &H74, &H04, &H46, &HE9, &HF2, &HFF
 $inline &H83, &HC7, &H03, &H59, &H5E, &H1F, &HE9, &HD5, &HFF, &H8A
 $inline &HD7, &HB6, &H00, &H8B, &HC8, &H86, &HCD, &HB5, &H00, &H8B
 $inline &HF2, &H01, &HCE, &H8A, &H5C, &H01, &H36, &H88, &H5E, &HFA
 $inline &H01, &HCE, &H8A, &H5C, &H01, &H36, &H88, &H5E, &HFB, &H01
 $inline &HCE, &H01, &HD6, &H8B, &H54, &H01, &H8B, &HC1, &HD1, &HE0
 $inline &HD1, &HE0, &H01, &HC8, &H01, &HD0, &H40, &H36, &H89, &H46
 $inline &HFC, &HE8, &H19, &HFF, &H36, &H8B, &H4E, &HFA, &HB5, &H00
 $inline &H01, &HCF, &H47, &H59, &H5E, &H1F, &HE9, &H8F, &HFF, &H58
 $inline &H58, &H1F, &H5D
End Sub
sub LAYAR19 inline
 $inline &HB8, &H13, &H00, &HCD, &H10
End sub

screen 0
dim dynamic font%(410)
call layar19
call instalfont(font%(0),"e:\aaprojek\livan.lff"+chr$(0), erno%)
call vhuruf(font%(0),&Ha000,0,0,"This must be fast, because i'm use inline",10)