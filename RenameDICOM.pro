pro RenameDICOM,  GROUP_LEADER=bMain

COMMON VAR, tblAdr, txtCat, txtFormat, lstNameElement, lstTemplate, btnRename,lblStatus, origPaths, newPaths, pathType, thisPath, defPath, $
  catTemplate, fileTemplate, catTemp, fileTemp, structTemp, tempElements, tagGroups, tagElements, formatElements,defCatOrFile, btnPutAllinOne

  COMPILE_OPT hidden
  
  origPaths=''
  newPaths=''
  pathType=0; 1=subdirectories, 2=DICOM-files
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('RenameDICOM'))+'\' 
  
  RESTORE, thisPath+'configRename.dat'
  
  defPath=structTemp.(0).defPath

  tempElements=structTemp.(0).tempElements
  tagGroups=structTemp.(0).tagGroups
  tagElements=structTemp.(0).tagElements
  defTempNo=structTemp.(0).selDefault
  formatElements=structTemp.(defTempNo+1).Formats
  catTemp=structTemp.(defTempNo+1).CatElem
  fileTemp=structTemp.(defTempNo+1).FileElem

  nTemp=N_TAGS(structTemp)-1
  tempNames=STRARR(nTemp)
  FOR i=0, nTemp-1 DO tempNames(i)=structTemp.(i+1).name 

  bMain = WIDGET_BASE(TITLE='Rename DICOM files and folders', MBAR=bar, /COLUMN, XSIZE=950, YSIZE=850, XOFFSET=50, YOFFSET=50,/TLB_KILL_REQUEST_EVENTS)
  
  ;menu
  file_menu=WIDGET_BUTTON(bar, VALUE='File', /MENU)
  btn_settings=WIDGET_BUTTON(file_menu, VALUE='Settings', UVALUE='settings')
  btn_exit=WIDGET_BUTTON(file_menu, VALUe='Exit', UVALUE='Exit', ACCELERATOR='Ctrl+X', /SEPARATOR)
  help_menu=WIDGET_BUTTON(bar, VALUE='Help', /MENU)
  btn_info=WIDGET_BUTTON(help_menu, VALUE='Wiki on GitHub.com', UVALUE='help')
  btn_about=WIDGET_BUTTON(help_menu, VALUE='About RenameDICOM', UVALUE='about')

  ;Name templates
  lblInfoName=WIDGET_LABEL(bMain,VALUE='Name format', FONT="Arial*Bold*18")
  bNameConstruct=WIDGET_BASE(bMAin, /COLUMN, FRAME=1, XSIZE=950, YSIZE=190)
  
  bNC2=WIDGET_BASE(bNameConstruct, /ROW)

  lstNameElement=WIDGET_LIST(bNC2, VALUE=tempElements, UVALUE='lstNameElement', SCR_YSIZE=180)
  
  bNCcol=WIDGET_BASE(bNC2, /COLUMN)
  
  bTemplate=WIDGET_BASE(bNCcol, /ROW)
  lblTemplate=WIDGET_LABEL(bTemplate, VALUE='Select template:')
  lstTemplate=WIDGET_DROPLIST(bTemplate, VALUE=tempNames, UVALUE='lstTemplate', XSIZE=100)
  btnSaveTemplate=WIDGET_BUTTON(bTemplate, VALUE='save.bmp', /BITMAP, TOOLTIP='Save current Name formats as template', UVALUE='saveTemp')
  btnDelTemplate=WIDGET_BUTTON(bTemplate, VALUE='delete.bmp', /BITMAP, TOOLTIP='Delete selected template', UVALUE='delTemp')
  
  lblInfoName=WIDGET_LABEL(bNCcol,VALUE='Double-click on element in the list to add element to the name template.', /ALIGN_LEFT)

  bNCrow=WIDGET_BASE(bNCcol, /ROW)
  defCatOrFile=CW_BGROUP(bNCrow, ['Subfolder name','File name'], UVALUE='defCatOrFile', SET_VALUE=0, /COLUMN, /EXCLUSIVE, /RETURN_INDEX)
  bStrings=WIDGET_BASE(bNCrow, /COLUMN)
  bStringsCat=WIDGET_BASE(bStrings, /ROW)
  bStringsFile=WIDGET_BASE(bStrings, /ROW)
  
  catTemplate=WIDGET_TEXT(bStringsCat, VALUE=catStringTemplate, SCR_XSIZE=650, XSIZE=200)
  fileTemplate=WIDGET_TEXT(bStringsFile, VALUE=fileStringTemplate, SCR_XSIZE=650, XSIZE=200)
  btnCatEmpty=WIDGET_BUTTON(bStringsCat, VALUE='<-', UVALUE='catPop', TOOLTIP='Remove last element from name format')
  btnFileEmpty=WIDGET_BUTTON(bStringsFile, VALUE='<-', UVALUE='filePop', TOOLTIP='Remove last element from name format')
  
  i=structTemp.(0).selDefault
  stringTemps=stringOfTemplate(structTemp, i, tempElements)
  WIDGET_CONTROL, catTemplate, SET_VALUE=stringTemps(0)
  WIDGET_CONTROL, fileTemplate, SET_VALUE=stringTemps(1)
  
  mlF=WIDGET_LABEL(bNCcol, VALUE='')
  
  ;Format coding
  bFormat=WIDGET_BASE(bNCcol, /ROW)
  lblFormat=WIDGET_LABEL(bFormat, VALUE='IDL format code for selected element in list:')
  txtFormat=WIDGET_TEXT(bFormat, VALUE='',/EDITABLE)
  btnFormat=WIDGET_BUTTON(bFormat, VALUE='Apply', UVALUE='applyFormat')
  lblFormatInfo=WIDGET_LABEL(bFormat, VALUE='Text: a<#letters or 0=all>, Integer: i<#digits>, Float: f<#integer digits>.<#decimals>')
  
  lblMlm=WIDGET_LABEL(bMain, VALUE='', YSIZE=20)
  
  bBrowse=WIDGET_BASE(bMain, /ROW, XSIZE=900, SCR_XSIZE=500)
  lblCat=WIDGET_LABEL(bBrowse, VALUE='Selected folder: ', FONT="Arial*Bold*18")
  txtCat=WIDGET_TEXT(bBrowse, XSIZE=87)
  btnBrowse=WIDGET_BUTTON(bBrowse, VALUE='Browse',UVALUE='browse', XSIZE=50)
  btnPutAllinOne=WIDGET_BUTTON(bBrowse, VALUE='Move all files in subfolders to selected folder', UVALUE='putAllinOne')
  
  lblMlm2=WIDGET_LABEL(bMain, VALUE='', YSIZE=20)
  
  bTable=WIDGET_BASE(bMain, /ROW)
  mlmTbl=WIDGET_LABEL(bTable, VALUE='', XSIZE=70)
  rownames=['Original name', 'Suggested name']
  tblAdr = WIDGET_TABLE(bTable, SCR_XSIZE=700, XSIZE=2, YSIZE=200, SCR_YSIZE=500, /NO_ROW_HEADERS, column_widths=[350,350], column_labels=rownames, ALIGNMENT=1)
  lblMl3=WIDGET_LABEL(bTable, VALUE='', XSIZE=50)
  bSide=WIDGET_BASE(bTable, /COLUMN)
  lblS=WIDGET_LABEL(bSide, VALUe='', YSIZE=50)
  btnViewFirstCat=WIDGET_BUTTON(bSide, VALUE='Test 10 first', UVALUE='firstFolders')
  btnUpdateName = WIDGET_BUTTON(bSide, VALUE='Generate names', UVALUE='update')
  btnRename = WIDGET_BUTTON(bSide, VALUE='Rename', SENSITIVE=0, XSIZE=80, UVALUE='rename')
  
  bBottom=WIDGET_BASE(bMain, /row, XSIZE=950)
  lblStatus = WIDGET_LABEL(bBottom, VALUE='Status:', SCR_XSIZE=950, XSIZE=650, FRAME=1, /DYNAMIC_RESIZE)

  WIDGET_CONTROL, bMain, /REALIZE 
  XMANAGER, 'RenameDICOM', bMain 
end


