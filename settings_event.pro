pro settings_event, event

  COMMON SETT
  COMMON var
  COMPILE_OPT hidden
  
  WIDGET_CONTROL, event.ID, GET_UVALUE=uval

  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
    's_browse':BEGIN
      adr=dialog_pickfile(PATH=defPath,/DIRECTORY, /READ, TITLE='Select default folder')
      IF adr(0) NE '' THEN defPath=FILE_DIRNAME(adr(0))
      WIDGET_CONTROL, txtAdr, SET_VALUE=adr(0)
      END
    'tblElem': BEGIN
       stop
      END
    's_cancel': WIDGET_CONTROL, Event.top, /DESTROY
    's_save':BEGIN
      WIDGET_CONTROL, txtAdr, GET_VALUE=sAdr
      structTemp.(0).defPath=sAdr
      WIDGET_CONTROL, tblElem, GET_VALUE=tblNew
      box=[$
        '1, BASE,, /ROW', $
        '2, LABEL, The same DICOM tag list is used for all templates. Use just this session or Save to config file?', $
        '1, BASE,, /ROW', $
        '0, BUTTON, Use, QUIT, TAG=Use',$
        '2, BUTTON, Save, QUIT, TAG=Save']
      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Use or save?', XSIZE=250, YSIZE=100, FOCUSNO=1)
      tempElements=TRANSPOSE(tblNew[0,*])
      groups=TRANSPOSE(tblNew[1,*])
      READS, groups, tagGroups, FORMAT='(Z)'
      elements=TRANSPOSE(tblNew[2,*])
      READS, elements, tagElements, FORMAT='(Z)'
      IF res.Save THEN BEGIN
            structTemp.(0).tempElements=tempElements
            structTemp.(0).tagGroups=tagGroups
            structTemp.(0).tagElements=tagElements
            SAVE, structTemp, FILENAME=thisPath+'configRename.dat'
      ENDIF
      WIDGET_CONTROL, Event.top, /DESTROY
      END
    ELSE:
    ENDCASE
  ENDIF

end