pro MakeExeRenameDICOM
exePath='I:\Felles\Straalevern\Kvalitetskontroll\IDL_programmer\Exe\'
thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('RenameDICOM'))+'\' 
MAKE_RT, 'RenameDICOM', exePath, /OVERWRITE, SAVEFILE=thisPath+'RenameDICOM.sav', /VM, /WIN32

;----------------------
;RenameDICOM.ini (change show to false)
;[DIALOG]
;Show=FALSE
;BackColor=&H6B1F29
;Caption=IDL Virtual Machine Application
;Picture=.\splash.bmp
;DefaultAction=.\IDL71\bin\bin.x86\idlrt.exe -vm=RenameDICOM.sav
;
;[BUTTON1]
;Show=True
;Caption=TestObjGraphics
;Action=.\IDL71\bin\bin.x86\idlrt.exe -vm=RenameDICOM.sav
;
;[BUTTON2]
;Show=True
;Caption=Exit
;Action=Exit
;
;[BUTTON3]
;Show=False
;Caption=
;Action=
;
;[BUTTON4]
;Show=False
;Caption=
;Action=
;-----------------------------
;autorun.inf
;[autorun]
;open = RenameDICOM.exe
;icon= RenameDICOM.ico


end