;RenameDICOM - renaming DICOM files based on header information
;Copyright (C) 2016  Ellen Wasbo, Stavanger University Hospital, Norway
;ellen@wasbo.no
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License version 2
;as published by the Free Software Foundation.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

function newFileName, path, tgroups, telements, formatTemp
  newpath=''
  dcm=QUERY_DICOM(path)
  IF dcm EQ 1 THEN BEGIN
    o=obj_new('idlffdicom')
    t=o->read(path)
    
    nElem=N_ELEMENTS(telements)
    
    nameStr=''  
    FOR i=0, nElem-1 DO BEGIN
      
      test=o->GetReference(tgroups(i),telements(i))
      IF test(0) NE -1 THEN BEGIN
        test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)
        
        stest=size(test_peker, /TNAME)
        IF stest EQ 'POINTER' THEN BEGIN
          namePart=*(test_peker[0])
          
          CATCH, Error_status
          IF Error_status NE 0 THEN BEGIN
            formatTemp(i)='(i0)'
            CATCH, /CANCEL
          ENDIF
          nameParts=STRSPLIT(namePart(0),'\',/EXTRACT)      
          formats=STRSPLIT(STRMID(formatTemp(i), 1, strlen(formatTemp(i))-2),'\',/EXTRACT)
          nP=N_ELEMENTS(nameParts)
          IF nP EQ N_ELEMENTS(formats) AND nP GT 1 THEN BEGIN
            namePart=''
            FOR p=0, nP-1 DO BEGIN
              IF formats(p) NE '_' THEN namePart=namePart+STRING(nameParts(p), FORMAT='('+formats(p)+')')
            ENDFOR
          ENDIF ELSE namePart=STRING(namePart(0), FORMAT=formatTemp(i))
          ;namePart=IDL_VALIDNAME(STRING(namePart(0), FORMAT=formatTemp(i)),/CONVERT_ALL)
          IF i EQ 0 THEN nameStr=namePart ELSE nameStr=nameStr+'_'+namePart
        ENDIF
       
      ENDIF
    ENDFOR
    
    obj_destroy,o
    
    ;nameStr=STRJOIN(STRSPLIT(nameStr,'-',/EXTRACT,/PRESERVE_NULL),'_');change minus to underscore
    nameStr=STRJOIN(STRSPLIT(nameStr,'*',/EXTRACT,/PRESERVE_NULL),'X');change * to X (format code not ideal)
    nameStr=STRJOIN(STRSPLIT(nameStr,' ',/EXTRACT),'_');change space to underscore
    ;nameStr=STRJOIN(STRSPLIT(nameStr,'\',/EXTRACT),'_');change slash to underscore
    ;nameStr=STRJOIN(STRSPLIT(nameStr,'/',/EXTRACT),'_');change slash to underscore  
    ;nameStr=STRJOIN(STRSPLIT(nameStr,'?',/EXTRACT),'_')
    nameStr=STRJOIN(STRSPLIT(nameStr,'[-/\?#%&{}`<>$!:@+|=]',/EXTRACT, /REGEX),'_')
      
    arr=STRSPLIT(path,'\',/EXTRACT)
    last=n_elements(arr)-1
    newpath=STRJOIN(arr[0:last-1],'\')+'\'+nameStr+'.dcm'
  ENDIF
  
  return, newpath
end

function newCatName, path, tgroups, telements, formatTemp
  newpath=''
  Spawn, 'dir '  + '"'+path+'"' + '*'+ '/b /a-d', res; files only,may not work in standalone version (makeRT)
  ;res=FILE_SEARCH(path,'I*')

  IF res(0) NE '' THEN BEGIN
      res=path+res(sort(res))
      nn=N_ELEMENTS(res)
      dcm=-1
      counter=0
      FOR n=0, nn-1 DO BEGIN
        dcm=QUERY_DICOM(res(n))
        IF dcm EQ 1 THEN BREAK ELSE counter=counter+1
      ENDFOR
      IF dcm GT 0 THEN BEGIN
        o=obj_new('idlffdicom')
        t=o->read(res(counter))
        
        nElem=N_ELEMENTS(telements)

        nameStr=''
        FOR i=0, nElem-1 DO BEGIN

          test=o->GetReference(tgroups(i),telements(i))
          IF test(0) NE -1 THEN BEGIN
            test_peker=o->GetValue(REFERENCE=test[0],/NO_COPY)

            stest=size(test_peker, /TNAME)
            IF stest EQ 'POINTER' THEN BEGIN
              namePart=*(test_peker[0])
              nameParts=STRSPLIT(namePart(0),'\',/EXTRACT)
              formats=STRSPLIT(STRMID(formatTemp(i), 1, strlen(formatTemp(i))-2),'\',/EXTRACT)
              nP=N_ELEMENTS(nameParts)
              IF nP EQ N_ELEMENTS(formats) AND nP GT 1 THEN BEGIN
                namePart=''
                FOR p=0, nP-1 DO BEGIN
                  IF formats(p) NE '_' THEN namePart=namePart+STRING(nameParts(p), FORMAT='('+formats(p)+')')
                ENDFOR
              ENDIF ELSE namePart=STRING(namePart(0), FORMAT=formatTemp(i))
              ;namePart=IDL_VALIDNAME(STRING(namePart(0), FORMAT=formatTemp(i)),/CONVERT_ALL)
              IF i EQ 0 THEN nameStr=namePart ELSE nameStr=nameStr+'_'+namePart
            ENDIF

          ENDIF
        ENDFOR
        
        obj_destroy,o
        
        nameStr=IDL_VALIDNAME(nameStr,/CONVERT_ALL)
        nameStr=STRJOIN(STRSPLIT(nameStr,'_',/EXTRACT),'_');remove first and last underscore
        arr=STRSPLIT(path,'\',/EXTRACT)
        last=n_elements(arr)-1
        newpath=STRJOIN(arr[0:last-1],'\')+'\'+nameStr+'\'
        
      ENDIF
  ENDIF; no files found
  
  return, newpath
end

function getUniqPaths, origPaths,newPaths,pathType

  sortedOrder=sort(newPaths)
  modNewPaths=newPaths(sortedOrder)
  modOrigPaths=origPaths(sortedOrder)
  u=UNIQ(modNewPaths)
  IF N_ELEMENTS(u) NE N_ELEMENTS(modNewPaths) THEN BEGIN
    FOR i=0, N_ELEMENTS(modNewPaths)-1 DO BEGIN
      eqPaths=WHERE(modNewPaths EQ modNewPaths(i), nP)

      IF nP GT 1 THEN BEGIN
        FOR j=0, nP-1 DO BEGIN
          IF pathType EQ 1 THEN modNewPaths(eqPaths(j))= FILE_DIRNAME(modNewPaths(eqPaths(j)))+'\'+FILE_BASENAME(modNewPaths(eqPaths(j))) + '_' + STRING(j, FORMAT='(i02)')+'\' $
          ELSE modNewPaths(eqPaths(j))= FILE_DIRNAME(modNewPaths(eqPaths(j)))+'\'+FILE_BASENAME(modNewPaths(eqPaths(j)),'.dcm')+'_'+STRING(j, FORMAT='(i02)')+'.dcm'
        ENDFOR
      ENDIF
    ENDFOR
  ENDIF
  sortedOrder=sort(modNewPaths)
  modNewPaths=modNewPaths(sortedOrder)
  modOrigPaths=modOrigPaths(sortedOrder)

  structPaths=CREATE_STRUCT('origPaths',modOrigPaths, 'newPaths',modNewPaths)
  return, structPaths
end

pro RenameDICOM_event, event

  COMMON var
  COMPILE_OPT hidden
  
  WIDGET_CONTROL, event.ID, GET_UVALUE=uval
  
  IF N_ELEMENTS(uval) GT 0 THEN BEGIN
    CASE uval OF
      'help': XDISPLAYFILE,thisPath+'info.txt',DONE_BUTTON='Close',TITLE='Inforation',/MODAL
      'Exit': WIDGET_CONTROL, event.top, /DESTROY
      'lstNameElement': BEGIN
        IF event.clicks EQ 2 THEN BEGIN  
          WIDGET_CONTROL, defCatOrFile, GET_VALUE=defa
          ;add element to numbered list and element to string
          IF defa EQ 0 THEN BEGIN; folder          
            IF N_ELEMENTS(catTemp) GT 0 THEN BEGIN              
              already=WHERE(catTemp EQ event.index)
              IF already(0) EQ -1 THEN BEGIN
                catTemp=[catTemp,event.index]
                WIDGET_CONTROL, catTemplate, GET_VALUE=stringTemp
                WIDGET_CONTROL, catTemplate, SET_VALUE=stringTemp+'_<'+tempElements(event.index)+'>'
              ENDIF
            ENDIF ELSE BEGIN
              catTemp=event.index
              WIDGET_CONTROL, catTemplate, SET_VALUE='<'+tempElements(event.index)+'>'
            ENDELSE        
           
          ENDIF ELSE BEGIN; file
            IF N_ELEMENTS(fileTemp) GT 0 THEN BEGIN
              already=WHERE(fileTemp EQ event.index)
              IF already(0) EQ -1 THEN BEGIN
                fileTemp=[fileTemp,event.index]
                WIDGET_CONTROL, fileTemplate, GET_VALUE=stringTemp
                WIDGET_CONTROL, fileTemplate, SET_VALUE=stringTemp+'_<'+tempElements(event.index)+'>'
              ENDIF
            ENDIF ELSE BEGIN
              fileTemp=event.index
              WIDGET_CONTROL, fileTemplate, SET_VALUE='<'+tempElements(event.index)+'>'
            ENDELSE
          ENDELSE
        ENDIF; event.clicks 2
        WIDGET_CONTROL, txtFormat, SET_VALUE=STRMID(formatElements(event.index), 1, strlen(formatElements(event.index))-2)
        END
       'catPop': BEGIN
        ;remove last element
        nTemp=N_ELEMENTS(catTemp)
        IF nTemp GT 1 THEN BEGIN
          catTemp=catTemp[0:nTemp-2]
          WIDGET_CONTROL, catTemplate, GET_VALUE=stringTemp
          cutString=STRSPLIT(stringTemp,'_',/EXTRACT)
          WIDGET_CONTROL, catTemplate, SET_VALUE=STRJOIN(cutString[0:nTemp-2],'_')
        ENDIF ELSE BEGIN
          catTemp=!NULL
          WIDGET_CONTROL, catTemplate, SET_VALUE=''
        ENDELSE
        END 
       'filePop': BEGIN
          ;remove last element
          nTemp=N_ELEMENTS(fileTemp)
          IF nTemp GT 1 THEN BEGIN
            fileTemp=fileTemp[0:nTemp-2]
            WIDGET_CONTROL, fileTemplate, GET_VALUE=stringTemp
            cutString=STRSPLIT(stringTemp,'_',/EXTRACT)
            WIDGET_CONTROL, fileTemplate, SET_VALUE=STRJOIN(cutString[0:nTemp-2],'_')
          ENDIF ELSE BEGIN
            fileTemp=!NULL
            WIDGET_CONTROL, fileTemplate, SET_VALUE=''
          ENDELSE
        END
       'catGetName': BEGIN; use default name template
          RESTORE, thisPath+'configSort.dat'
          catTemp=catDefTemp
          nTemp=N_ELEMENTS(catTemp)
          IF nTemp GT 0 THEN BEGIN
            strTemp=STRARR(nTemp)
            FOR i=0, nTemp-1 DO strTemp(i)=tempElements(catTemp(i))
            WIDGET_CONTROL, catTemplate, SET_VALUE='<'+STRJOIN(strTemp,'>_<')+'>'
          ENDIF ELSE WIDGET_CONTROL, catTemplate, SET_VALUE=''
       END
       'catSetName': BEGIN;set default name template
          RESTORE, thisPath+'configSort.dat'
          catDefTemp=catTemp
          SAVE, catDefTemp, fileDefTemp, FILENAME=thisPath+'configSort.dat'
        END
       'fileGetName':BEGIN; use default name template
          RESTORE, thisPath+'configSort.dat'
          fileTemp=fileDefTemp
          nTemp=N_ELEMENTS(fileTemp)
          IF nTemp GT 0 THEN BEGIN
            strTemp=STRARR(nTemp)
            FOR i=0, nTemp-1 DO strTemp(i)=tempElements(fileTemp(i))
            WIDGET_CONTROL, fileTemplate, SET_VALUE='<'+STRJOIN(strTemp,'>_<')+'>'
          ENDIF ELSE WIDGET_CONTROL, fileTemplate, SET_VALUE=''
       END
       'fileSetName':BEGIN;set default name template
          RESTORE, thisPath+'configSort.dat'
          fileDefTemp=fileTemp
          SAVE, catDefTemp, fileDefTemp, FILENAME=thisPath+'configSort.dat'
        END
        'browse':BEGIN
          WIDGET_CONTROL, lblStatus, SET_VALUE=''
          adr=dialog_pickfile(PATH=defPath,/DIRECTORY, /READ, TITLE='Select folder whith DICOM files or subfolders')
          IF adr(0) NE '' THEN defPath=FILE_DIRNAME(adr(0))
          WIDGET_CONTROL, txtCat, SET_VALUE=adr(0)
          WIDGET_CONTROL, tblAdr, SET_VALUE=STRARR(2,200)
          ;update=1
          END
        'putAllinOne':BEGIN
          WIDGET_CONTROL, txtCat, GET_VALUE=adr
          Spawn, 'dir '  + '"'+adr(0)+'\"' + '*'+ '/b /s', res; both files and directories
          origPaths=res(sort(res))
          IF origPaths(0) NE '' THEN BEGIN
              nnn=N_ELEMENTS(origPaths)
              FOR i =0,nnn-1 DO BEGIN
                resFI=FILE_INFO(origPaths(i))
                IF resFI.DIRECTORY EQ 1 THEN origPaths(i)='' ELSE BEGIN
                  dcm=QUERY_DICOM(origPaths(i))
                  IF dcm EQ 0 THEN BEGIN
                    origPaths(i)=''
                  ENDIF ELSE BEGIN
                    IF FILE_BASENAME(origPaths(i)) EQ 'DICOMDIR' THEN origPaths(i)=''
                  ENDELSE
                ENDELSE
              ENDFOR

              notEmpty=WHERE(origPaths NE '')
              origPaths2=origPaths(notEmpty)
              origPaths=origPaths2
              nFiles=N_ELEMENTS(origPaths)
              newPaths=origPaths & newPaths[*]=''

              for i=0, nFiles-1 do begin
                filen=FILE_BASENAME(origPaths(i))
                tempname=adr(0)+ filen
                alreadyName=WHERE(newPaths EQ tempname)
                IF alreadyName(0) NE -1 THEN BEGIN
                  tempname=STRMID(tempname,0,STRLEN(tempname)-4)+'_'+STRING(N_ELEMENTS(alreadyName),FORMAT='(i03)')+'.dcm'
                ENDIF
                newPaths(i)=tempname
                file_move, origPaths(i), newPaths(i)
              endfor
            ENDIF
          END
        'firstFolders':update=10
        'update':update=1
        'rename':rename=1
        'applyFormat': BEGIN
            WIDGET_CONTROL, txtFormat, GET_VALUE=newFormat
            sel=WIDGET_INFO(lstNameElement, /LIST_SELECT)
            formatElements(sel(0))='('+newFormat(0)+')'
            END
      Else:
    ENDCASE
  ENDIF; uval defined
  
  ;******************* Exit program ***********************
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
  ENDIF
  
  ;******************* Finding suggested names ***********************
  IF N_ELEMENTS(update) GT 0 THEN BEGIN
    IF update GE 1 THEN BEGIN
    WIDGET_CONTROL, /HOURGLASS
    WIDGET_CONTROL, txtCat, GET_VALUE=adr
    Spawn, 'dir '  + '"'+adr(0)+'"' + '*'+ '/b /ad', res; directories only
    origPaths=adr(0)+res(sort(res)) 
    ;origPaths=file_search(adr, '*', /TEST_DIRECTORY); find all subdirectories
    delimPos=STRPOS(origPaths,'\', /REVERSE_SEARCH)
    IF delimPos(0) NE 0 THEN origPaths=origPaths+'\' 
    IF res(0) NE '' THEN BEGIN
    
      pathType=1
      IF N_ELEMENTS(catTemp) GT 0 THEN BEGIN
        newPaths=origPaths & newPaths[*]=''
  
        IF update EQ 10 THEN ncats=MIN([10,n_elements(origPaths)]) ELSE ncats=n_elements(origPaths)
        displayTable=STRARR(2,200)
        IF ncats LT 200 THEN displayTable[0,0:ncats-1]=FILE_BASENAME(origPaths[0:ncats-1]) ELSE BEGIN
          displayTable[0,*]=FILE_BASENAME(origPaths[0:199])
          sv=DIALOG_message('More than 200 subfolders. Only first 200 will appear in list.')
        ENDELSE
        WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable
        
        for i=0, ncats-1 do begin
          newPaths(i)=newCatName(origPaths(i),  tagGroups(catTemp), tagElements(catTemp),formatElements(catTemp))
          IF i LT 200 THEN displayTable(1,i)=FILE_BASENAME(newPaths(i)) ELSE displayTable[1,*]=FILE_BASENAME(newPaths[0:199])
          WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable
          WIDGET_CONTROL, lblStatus, SET_VALUE='Finding names from DICOM content - folder: '+displayTable(0,i)
        endfor
        If update EQ 1 THEN WIDGET_CONTROL, btnRename, SENSITIVE=1
        WIDGET_CONTROL, lblStatus, SET_VALUE=''
      ENDIF ELSE sv=DIALOG_MESSAGE('Name template cannot be empty.')

    ENDIF ELSE BEGIN ; no catalogues search for files
      pathType=2
      IF N_ELEMENTS(fileTemp) GT 0 THEN BEGIN
        Spawn, 'dir '  + '"'+adr(0)+'\"' + '*'+ '/b /a-d', res; files only,may not work in standalone version (makeRT)
        origPaths=adr(0)+res(sort(res))
        ;origPaths=file_search(adr, 'I*')
        IF origPaths(0) NE '' THEN BEGIN
          IF update EQ 10 THEN nFiles=MIN([10,N_ELEMENTS(origPaths)]) ELSE nFiles=N_ELEMENTS(origPaths)
          newPaths=origPaths & newPaths[*]=''
          displayTable=STRARR(2,200)
          IF nFiles LT 200 THEN displayTable[0,0:nFiles-1]=FILE_BASENAME(origPaths[0:nFiles-1]) ELSE BEGIN
            displayTable[0,0:199]=FILE_BASENAME(origPaths[0:199])
            sv=DIALOG_message('More than 200 files. Only first 200 will appear in list.')
          ENDELSE
          WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable
          
          for i=0, nFiles-1 do begin
          
            newPaths(i)=newFileName(origPaths(i), tagGroups(fileTemp), tagElements(fileTemp),formatElements(fileTemp))
            IF i LT 200 THEN displayTable(1,i)=FILE_BASENAME(newPaths(i)) ELSE displayTable[1,*]=FILE_BASENAME(newPaths[0:199])
            WIDGET_CONTROL, tblAdr, SET_VALUE=displayTable
            WIDGET_CONTROL, lblStatus, SET_VALUE='Finding names from DICOM content. Filenumber '+STRING(i,FORMAT='(i0)')+' / '+STRING(nFiles, FORMAT='(i0)')
          endfor
          IF update EQ 1 THEN WIDGET_CONTROL, btnRename, SENSITIVE=1
          WIDGET_CONTROL, lblStatus, SET_VALUE=''
        ENDIF ELSE sv=DIALOG_MESSAGE('Found no subfolders or DICOM images in this folder. NB: Cannot handle special charachters in the path.')
      ENDIF ELSE sv=DIALOG_MESSAGE('Name template cannot be empty.')
    ENDELSE
    ENDIF
    update=0
  ENDIF
  
  ;******************* Perform rename of catalogues or files ? ***********************
  IF N_ELEMENTS(rename) NE 0 THEN BEGIN
    IF rename EQ 1 THEN BEGIN
  
    ;unique names?
    modPaths=getUniqPaths(origPaths,newPaths,pathType)
    origPaths=modPaths.origPaths & newPaths=modPaths.newPaths

    u=UNIQ(newPaths)

    antEndret=0
    IF N_ELEMENTS(u) EQ N_ELEMENTS(newPaths) THEN BEGIN
    
      nP= n_elements(newPaths)   
      for i=0, nP-1 do begin
        IF newPaths(i) NE '' AND newPaths(i) NE origPaths(i) THEN BEGIN
          file_move, origPaths(i), newPaths(i)
          antEndret=antEndret+1
        ENDIF
        WIDGET_CONTROL, lblStatus, SET_VALUE='Renaming... '+STRING(i,FORMAT='(i0)')+' / '+STRING(nP, FORMAT='(i0)')
      endfor
      WIDGET_CONTROL, btnRename, SENSITIVE=0
      WIDGET_CONTROL, lblStatus, SET_VALUE='Finished renaming'
      WIDGET_CONTROL, tblAdr, SET_VALUE=STRARR(2,200)
      
      
      ; also rename files?
      IF pathType EQ 1 THEN BEGIN
        sv=DIALOG_MESSAGE('Also rename files?',/QUESTION)
        IF sv EQ 'Yes' THEN BEGIN
          WIDGET_CONTROL, /HOURGLASS
          pathType=2
          dirNames=newPaths
          nDirs=n_elements(dirNames)
          FOR i=0, nDirs-1 DO BEGIN
              Spawn, 'dir '  + '"'+dirNames(i)+'"' + '*'+ '/b /a-d', res; files only,may not work in standalone version (makeRT)
              origPaths=dirNames(i)+res(sort(res))
            ;origPaths=file_search(dirNames(i), 'I*')
            IF origPaths(0) NE '' THEN BEGIN
              nFiles=N_ELEMENTS(origPaths)
              WIDGET_CONTROL, lblStatus, SET_VALUE='Renaming '+STRING(nFiles,FORMAT='(i0)')+' files in directory '+STRING(i+1, FORMAT='(i0)')+'/'+STRING(nDirs, FORMAT='(i0)')
              newPaths=origPaths & newPaths[*]=''
              for j=0, nFiles-1 do newPaths(j)=newFileName(origPaths(j), tagGroups(fileTemp), tagElements(fileTemp), formatElements(fileTemp))
              modPaths=getUniqPaths(origPaths,newPaths,pathType)
              origPaths=modPaths.origPaths & newPaths=modPaths.newPaths
              u=UNIQ(newPaths)
              IF N_ELEMENTS(u) EQ N_ELEMENTS(newPaths) THEN BEGIN
              
                for k=0, n_elements(newPaths)-1 do begin
                  IF newPaths(k) NE '' AND origPaths(k) NE newPaths(k) THEN file_move, origPaths(k), newPaths(k)
                endfor
              ENDIF ELSE BEGIN
                sv=DIALOG_MESSAGE('Names are not unique using the selected name template. Only first unique file is renamed.')
                origPathsMod=origPaths(u)
                newPathsMod=newPaths(u)
                for k=0, n_elements(newPathsMod)-1 do begin
                  IF newPathsMod(k) NE '' AND newPaths(i) NE origPaths(i) THEN file_move, origPathsMod(k), newPathsMod(k)
                endfor
              ENDELSE
            ENDIF
          ENDFOR
        ENDIF
      ENDIF
      
      WIDGET_CONTROL, lblStatus, SET_VALUE='Finished renaming'
      newPaths[*]='' & origPaths[*]=''
      pathType=0
    ENDIF ELSE BEGIN
     sv=DIALOG_MESSAGE('Names are not unique using the selected name template. Only first unique file is renamed.')
                origPathsMod=origPaths(u)
               newPathsMod=newPaths(u)
              for k=0, n_elements(newPathsMod)-1 do begin
               IF newPathsMod(k) NE '' AND newPathsMod(k) NE origPathsMod(k) THEN file_move, origPathsMod(k), newPathsMod(k)
            endfor
           WIDGET_CONTROL, lblStatus, SET_VALUE='Finished renaming'
      newPaths[*]='' & origPaths[*]=''
      pathType=0
    ENDELSE
  ENDIF
  rename=0
ENDIF
end

