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



;config file structure of structures, first which template default, then all the templates
;
;
;defPath='C:\'
;tempElements=['Acquisition number','Series number','Series description','Protocol name','kVp','mAs','Slice thickness','Filter','Instance number','Slice position (z)', 'date','time','ImagePos']
;tagGroups=['0020'x,'0020'x,'0008'x,'0018'x,'0018'x,'0018'x,'0018'x,'0018'x,'0020'x,'0020'x,'0008'x,'0008'x,'0020'x]
;tagElements=['0012'x,'0011'x,'103E'x,'1030'x,'0060'x,'1152'x,'0050'x,'1210'x,'0013'x,'1041'x,'0022'x,'0032'x,'0032'x]
;
;settings=CREATE_STRUCT('selDefault',0,'defPath',defPath,'tempElements',tempElements,'tagGroups',tagGroups,'tagElements',tagElements)
;temp0=CREATE_STRUCT('name','default','catElem',[0,1,2],'fileElem',[8,9], 'formats', ['(i04)','(i05)','(a0)','(a0)','(i0)','(i0)','(f0.1)','(a0)','(i04)','(f0.1)','(i0)','(i6)','(_\_\f0.1)'])
;structTemp=CREATE_STRUCT('settings',settings,'T0',temp0)
;;
;IDL> help, structTemp.(0), /STRUCTURE
;   SELDEFAULT      INT              0
;   DEFPATH         STRING    'I:\Felles\Straalevern\Kvalitetskontroll\CT\'
;   TEMPELEMENTS    STRING    Array[13]
;   TAGGROUPS       INT       Array[13]
;   TAGELEMENTS     INT       Array[13]
;;
;;IDL> help, /STRUCTURE, structTemp.(1)
;   NAME            STRING    'default'
;   CATELEM         INT       Array[3]
;   FILEELEM        INT       Array[2]
;   FORMATS         STRING    Array[13]
end