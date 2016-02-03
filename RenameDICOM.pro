pro RenameDICOM,  GROUP_LEADER=bMain

COMMON VAR, tblAdr, txtCat, txtFormat, lstNameElement, btnRename,lblStatus, origPaths, newPaths, pathType, thisPath, defPath, $
  catTemplate, fileTemplate, catTemp, fileTemp, tempElements, tagGroups, tagElements, formatElements,defCatOrFile, btnPutAllinOne

  COMPILE_OPT hidden
  
  origPaths=''
  newPaths=''
  pathType=0; 1=subdirectories, 2=DICOM-files
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('RenameDICOM'))+'\' 
  ;stop;catDefTemp=[0,1,2];fileDefTemp=[7,8];save, catDefTemp, fileDefTemp, FILENAME=thisPath+'configSort.dat'
  
  defPath='I:\Felles\Straalevern\Kvalitetskontroll\CT\'

  tempElements=['Acquisition number','Series number','Series description','Protocol name','kVp','mAs','Slice thickness','Filter','Instance number','Slice position (z)', 'date','time','ImagePos']
  tagGroups=['0020'x,'0020'x,'0008'x,'0018'x,'0018'x,'0018'x,'0018'x,'0018'x,'0020'x,'0020'x,'0008'x,'0008'x,'0020'x]
  tagElements=['0012'x,'0011'x,'103E'x,'1030'x,'0060'x,'1152'x,'0050'x,'1210'x,'0013'x,'1041'x,'0022'x,'0032'x,'0032'x]
  formatElements=['(i04)','(i05)','(a0)','(a0)','(i0)','(i0)','(f0.1)','(a0)','(i04)','(f0.1)','(i0)','(i6)','(_\_\f0.1)']

  RESTORE, thisPath+'configRename.dat'
  catTemp=catDefTemp & fileTemp=fileDefTemp
  nTemp=N_ELEMENTS(catTemp)
  IF nTemp GT 0 THEN BEGIN
    strTemp=STRARR(nTemp)
    FOR i=0, nTemp-1 DO strTemp(i)=tempElements(catTemp(i))
    catStringTemplate='<'+STRJOIN(strTemp,'>_<')+'>'
  ENDIF ELSE catStringTemplate=''
  nTemp=N_ELEMENTS(fileTemp)
  IF nTemp GT 0 THEN BEGIN
    strTemp=STRARR(nTemp)
    FOR i=0, nTemp-1 DO strTemp(i)=tempElements(fileTemp(i))
    fileStringTemplate='<'+STRJOIN(strTemp,'>_<')+'>'
  ENDIF ELSE fileStringTemplate=''

  bMain = WIDGET_BASE(TITLE='Rename DICOM files and folders', MBAR=bar, /COLUMN, XSIZE=1000, YSIZE=800, XOFFSET=100, YOFFSET=100,/TLB_KILL_REQUEST_EVENTS)
  
  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  btn_exit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='Exit', ACCELERATOR='Ctrl+X', /SEPARATOR)
  help_menu=WIDGET_BUTTON(bar, VALUE='Help', /MENU)
  btn_info=WIDGET_BUTTON(help_menu, VALUE='How it works...', UVALUE='help')

  
  bNameConstruct=WIDGET_BASE(bMAin, /COLUMN)
  lblInfoName=WIDGET_LABEL(bNameConstruct,VALUE='Double-click on element in the list to add element to the name template.', /ALIGN_LEFT)

  bNC2=WIDGET_BASE(bNameConstruct, /ROW)

  lstNameElement=WIDGET_LIST(bNC2, VALUE=tempElements, UVALUE='lstNameElement', SCR_YSIZE=160)
  bNCcol=WIDGET_BASE(bNC2, /COLUMN)
  bNCrow=WIDGET_BASE(bNCcol, /ROW)
  defCatOrFile=CW_BGROUP(bNCrow, ['Subfolder name','File name'], UVALUE='defCatOrFile', SET_VALUE=0, /COLUMN, /EXCLUSIVE, /RETURN_INDEX)
  bStrings=WIDGET_BASE(bNCrow, /COLUMN)
  bStringsCat=WIDGET_BASE(bStrings, /ROW)
  bStringsFile=WIDGET_BASE(bStrings, /ROW)
  
  catTemplate=WIDGET_TEXT(bStringsCat, VALUE=catStringTemplate, SCR_XSIZE=550, XSIZE=200)
  fileTemplate=WIDGET_TEXT(bStringsFile, VALUE=fileStringTemplate, SCR_XSIZE=550, XSIZE=200)
  btnCatEmpty=WIDGET_BUTTON(bStringsCat, VALUE='<-', UVALUE='catPop')
  btnFileEmpty=WIDGET_BUTTON(bStringsFile, VALUE='<-', UVALUE='filePop')
  btnCatGetName=WIDGET_BUTTON(bStringsCat, VALUE='Get default', UVALUE='catGetName')
  btnCatSetName=WIDGET_BUTTON(bStringsCat, VALUE='Set default', UVALUE='catSetName')
  btnFileGetName=WIDGET_BUTTON(bStringsFile, VALUE='Get default', UVALUE='fileGetName')
  btnFileSetName=WIDGET_BUTTON(bStringsFile, VALUE='Set default', UVALUE='fileSetName')
  
  mlF=WIDGET_LABEL(bNCcol, VALUE='')
  
  bFormat=WIDGET_BASE(bNCcol, /ROW)
  lblFormat=WIDGET_LABEL(bFormat, VALUE='IDL format code for selected element:')
  txtFormat=WIDGET_TEXT(bFormat, VALUE='',/EDITABLE)
  btnFormat=WIDGET_BUTTON(bFormat, VALUE='Apply', UVALUE='applyFormat')
  lblFormatInfo=WIDGET_LABEL(bFormat, VALUE='Text: a<#letters or 0=all>, Integer: i<#digits>, Float: f<#integer digits>.<#decimals>')
  
  
  bBrowse=WIDGET_BASE(bMain, /ROW, XSIZE=900, SCR_XSIZE=500)
  lblCat=WIDGET_LABEL(bBrowse, VALUE='Selected folder: ')
  txtCat=WIDGET_TEXT(bBrowse, XSIZE=100)
  btnBrowse=WIDGET_BUTTON(bBrowse, VALUE='Browse',UVALUE='browse', XSIZE=50)
  btnPutAllinOne=WIDGET_BUTTON(bBrowse, VALUE='Move all files in subfolders to selected folder', UVALUE='putAllinOne')
  
  bTable=WIDGET_BASE(bMain, /ROW)
  rownames=['Original name', 'Suggested name']
  tblAdr = WIDGET_TABLE(bTable, SCR_XSIZE=800, XSIZE=2, YSIZE=200, SCR_YSIZE=500, column_widths=[350,350], column_labels=rownames, ALIGNMENT=1)
  bSide=WIDGET_BASE(bTable, /COLUMN)
  btnViewFirstCat=WIDGET_BUTTON(bSide, VALUE='Test 10 first', UVALUE='firstFolders')
  btnUpdateName = WIDGET_BUTTON(bSide, VALUE='Generate names', UVALUE='update')
  btnRename = WIDGET_BUTTON(bSide, VALUE='Rename', SENSITIVE=0, XSIZE=80, UVALUE='rename')
  
  bBottom=WIDGET_BASE(bMain, /row, XSIZE=900)
  lblStatus = WIDGET_LABEL(bBottom, VALUE='Status:', SCR_XSIZE=900, XSIZE=650, FRAME=1, /DYNAMIC_RESIZE)

  WIDGET_CONTROL, bMain, /REALIZE 
  XMANAGER, 'RenameDICOM', bMain 
end


