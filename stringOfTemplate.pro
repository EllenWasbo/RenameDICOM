
;get strings for folder and file name formats
;struct = template struct
;i = template number to display
function stringOfTemplate, struct, selno, tempElements

stringTemp=STRARR(2)

nStruc=N_TAGS(struct)
IF selno LE nStruc-2 THEN BEGIN
  CatElem=struct.(selno+1).CatElem
  nTempElem=N_ELEMENTS(CatElem)
  IF nTempElem GT 0 THEN BEGIN
    strTemp=STRARR(nTempElem)
    FOR i=0, nTempElem-1 DO strTemp(i)=tempElements(CatElem(i))
    stringTemp(0)='<'+STRJOIN(strTemp,'>_<')+'>'
  ENDIF
  FileElem=struct.(selno+1).FileElem
  nTempElem=N_ELEMENTS(FileElem)
  IF nTempElem GT 0 THEN BEGIN
    strTemp=STRARR(nTempElem)
    FOR i=0, nTempElem-1 DO strTemp(i)=tempElements(FileElem(i))
    stringTemp(1)='<'+STRJOIN(strTemp,'>_<')+'>'
  ENDIF
ENDIF ELSE sv=DIALOG_MESSAGE('Error with numbering of templates....')
  
RETURN, stringTemp
end