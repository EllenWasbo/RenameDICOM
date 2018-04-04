pro settings, GROUP_LEADER = mainbase

COMMON SETT, txtAdr, tblElem
COMMON VAR

COMPILE_OPT hidden

settingsbox = WIDGET_BASE(TITLE='RenameDICOM settings', GROUP_LEADER=mainbase,  $
  /COLUMN, XSIZE=500, YSIZE=450, XOFFSET=150, YOFFSET=150, /MODAL)

ml1=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=20)

bAdr=WIDGET_BASE(settingsbox, /ROW)
lblAdr=WIDGET_LABEL(bAdr, VALUE='Default folder: ')
txtAdr=WIDGET_TEXT(bAdr, VALUE=structTemp.(0).defPath, XSIZE=52)
btnBrowse=WIDGET_BUTTON(bAdr, VALUE='Browse',UVALUE='s_browse', XSIZE=50)

ml2=WIDGET_LABEL(settingsbox, VALUE='', YSIZE=20)

bTbl=WIDGET_BASE(settingsbox, /ROW)
lblTbl=WIDGET_LABEL(bTbl, VALUE='', XSIZE=70)

tblContent=STRARR(4,N_ELEMENTS(tempElements))
tblContent[0,*]=TRANSPOSE(tempElements)
tblContent[1,*]=TRANSPOSE(STRING(tagGroups, FORMAT='(z04)'))
tblContent[2,*]=TRANSPOSE(STRING(tagElements, FORMAT='(z04)'))
tblContent[3,*]=TRANSPOSE(STRING(formatElements))

tblElem=WIDGET_TABLE(bTbl, VALUE=tblContent, XSIZE=4, YSIZE=N_ELEMENTS(tempElements), SCR_XSIZE=400, SCR_YSIZE=300,COLUMN_WIDTHS=[100,100,100,100], /NO_ROW_HEADERS, COLUMN_LABELS=['Element name','Tag group','Tag element','Formatcode'], /EDITABLE, /ALL_EVENTS)

bButtons=WIDGET_BASE(settingsbox, /ROW)
lblBtns0=WIDGET_LABEL(bButtons, VALUE='', XSIZE=70)
btnAdd=WIDGET_BUTTON(bButtons, VALUE='Add element', UVALUE='s_add')
lblBtns=WIDGET_LABEL(bButtons, VALUE='', XSIZE=150)
btnCancelSett=WIDGET_BUTTON(bButtons, VALUE='Cancel', UVALUE='s_cancel', XSIZE=50)
btnSaveSett=WIDGET_BUTTON(bButtons, VALUE='Use/save and Close', UVALUE='s_save', XSIZE=110)

WIDGET_CONTROL, settingsbox, /REALIZE
XMANAGER, 'settings', settingsbox

end