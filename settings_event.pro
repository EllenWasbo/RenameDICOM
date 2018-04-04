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
       ;stop
      END
    's_add': BEGIN
      WIDGET_CONTROL, tblElem, GET_VALUE=curTable
      tabSz=SIZE(curTable, /DIMENSIONS)
      newTable=STRARR(tabSz(0),tabSz(1)+1)
      newTable[*,0:tabSz(1)-1]=curTable
      newTable[*,tabSz(1)]=['new','0000','0000','(a0)']
      WIDGET_CONTROL, tblElem, TABLE_YSIZE=tabSz(1)+1, SET_VALUE=newTable, SET_TABLE_SELECT=[0,tabSz(1),3,tabSz(1)]
      WIDGET_CONTROL, tblElem, SET_TABLE_VIEW=[0, tabSz(1)-5]
      END
    's_cancel': WIDGET_CONTROL, Event.top, /DESTROY
    's_save':BEGIN
      WIDGET_CONTROL, txtAdr, GET_VALUE=sAdr
      structTemp.(0).defPath=sAdr
      WIDGET_CONTROL, tblElem, GET_VALUE=tblNew
      tabSz=SIZE(tblNew, /DIMENSIONS)
      equals=INTARR(tabSz(1))
      FOR i=0, tabSz(1)-1 DO IF ARRAY_EQUAL(tblNew[*,i],['new','0000','0000','(a0)']) THEN equals(i)=1
      IF TOTAL(equals) GT 0 THEN BEGIN
        IF ARRAY_EQUAL(equals(SORT(equals)), equals) THEN BEGIN
          tblNewNew=tblNew[*,0:tabSz(1)-1-TOTAL(equals)]
          tblNew=tblNewNew
          tabSz=SIZE(tblNew, /DIMENSIONS)
        ENDIF ELSE sv=DIALOG_MESSAGE('This should not be possible. New element not at end. Errors might occur later on with this if used or saved. Cancel recommended.',/ERROR)
      ENDIF
      box=[$
        '1, BASE,, /ROW', $
        '2, LABEL, The same DICOM tag list is used for all templates.', $
        '1, BASE,, /ROW', $
        '2, LABEL, Use edited list for this session or Save to config file?', $
        '1, BASE,, /ROW', $
        '0, BUTTON, Use, QUIT, TAG=Use',$
        '2, BUTTON, Save, QUIT, TAG=Save']
      res=CW_FORM_2(box, /COLUMN, TAB_MODE=1, TITLE='Use or save?', XSIZE=250, YSIZE=100, FOCUSNO=1)
      tempElements=TRANSPOSE(tblNew[0,*])
      formatElements=TRANSPOSE(tblNew[3,*])
      tagGroups=UINT(tagGroups) & tagElements=UINT(tagElements); problem if high numbered tags, int gets negative
      IF N_ELEMENTS(tagGroups) LT tabSz(1) THEN BEGIN
        diff=tabSz(1)-N_ELEMENTS(tagGroups)
        FOR i=0, diff-1 DO BEGIN
          tagGroups=[tagGroups,0]
          tagElements=[tagElements,0]
        ENDFOR
      ENDIF
      groups=TRANSPOSE(tblNew[1,*])
      READS, groups, tagGroups, FORMAT='(Z)'
      elements=TRANSPOSE(tblNew[2,*])
      READS, elements, tagElements, FORMAT='(Z)'
      IF res.Save THEN BEGIN
            sett=CREATE_STRUCT('selDefault',structTemp.(0).selDefault,'defPath',structTemp.(0).defPath,'tempElements',tempElements,'tagGroups',tagGroups,'tagElements',tagElements)
            newStruct=CREATE_STRUCT('settings',sett)
            nTemp=N_TAGS(structTemp)-1
            selNo=WIDGET_INFO(lstTemplate, /DROPLIST_SELECT)
            
            FOR i=1, nTemp DO BEGIN
              tempi=CREATE_STRUCT('name',structTemp.(i).name,'catElem',structTemp.(i).catElem,'fileElem',structTemp.(i).fileElem)
              
              IF i EQ selNo+1 THEN tempi=CREATE_STRUCT(tempi, 'formats', formatElements) ELSE BEGIN
                formTemp=structTemp.(i).formats
                IF N_ELEMENTS(formTemp) LT N_ELEMENTS(formatElements) THEN BEGIN
                  formNewi=formatElements
                  formNewi[0:N_ELEMENTS(formTemp)-1]=formTemp
                ENDIF
                tempi=CREATE_STRUCT(tempi, 'formats', formatElements)
              ENDELSE
              newStruct=CREATE_STRUCT(newStruct, 'T'+STRING(i, FORMAT='(i0)'), tempi)
            ENDFOR
            
            structTemp=newStruct
            SAVE, structTemp, FILENAME=thisPath+'configRename.dat'
      ENDIF
      WIDGET_CONTROL, Event.top, /DESTROY
      END
    ELSE:
    ENDCASE
  ENDIF

end