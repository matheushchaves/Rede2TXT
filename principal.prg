#Include "Fivewin.ch"

Function main()
         *
 			local cDirXLS := space(50), lConfirmar, cAction, cHistorico := space(50)
   		*
			REQUEST HB_Lang_PT
			REQUEST HB_CODEPAGE_DE850, HB_CODEPAGE_DEISO
			HB_LANGSELECT( 'PT' ) // Default language is now Portuguese
		   SetHandleCount(255)
		   SetBalloon( .T. )
		   SET DELETED ON
		   SET CENTURY ON
		   SET DATE BRIT
		   SET EPOCH TO 1980
		   SET MULTIPLE ON
		   *
		   SET 3DLOOK ON
		   SET SOFTSEEK OFF
		   SET CONFIRM ON
		   *
		   REQUEST DBFCDX
		   RDDSETDEFAULT("DBFCDX")
		   *
			lConfirmar := EDITVARS cDirXLS, cAction, cHistorico;
    							PROMPTS "Arquivo","SAY:PASTA","Histórico";
    							PICTURES "@!","","@!";
    							VALIDS {|oGet| iif(lIsDir(cDirXLS), .t. ,( cDirXLS := cGetDir32( "Selecione a Pasta", getenv("USERPROFILE")+"\Desktop" ),oGet:Refresh(),.f.))},;
    								    {|oGet| cDirXLS := cGetDir32( "Selecione a Pasta", getenv("USERPROFILE")+"\Desktop" ),oGet:Refresh()},;
    								    {|oGet| iif(Empty(cHistorico), MsgAlertReturn('Histórico em branco','Atenção',.f.), .t.) };
   							TITLE "Exportar registro Rede para CSV"
   		*					
   		MsgMeter( { | oMeter, oText, oDlg, lEnd | ;
             Export2CSV(cDirXLS, cHistorico, oMeter, oText, oDlg, @lEnd) },;
             "","Exportando registros para CSV")
         *
Function Export2CSV(cDirXLS, cHistorico, oMeter, oText, oDlg, lEnd)
			local aFiles := Array( ADir( cDirXLS+"\*.*" ) )
			local cFile := '', nFile := 1, nSucesso := 0, nFalha := 0				
   		*
			ADir( cDirXLS+"\*.*", aFiles )
			*
			oMeter:SetTotal(len(aFiles))
			
   		for each cFile in aFiles
   		   oDlg:SetText(cFile)
   			if lower(cFileExt(cFile)) == "xls" .or. lower(cFileExt(cFile)) == "xlsx" 
   				if XLSToTxt(cDirXLS+"\"+cFile, cHistorico, oText) 
   					nSucesso++
  					else
  					   nFalha++
					endif
				endif
   		   SysWait(0.1)
				oMeter:Set(nFile++)
				if lEnd
					exit
				endif
   		next
			if nSucesso == 0 .and. nFalha == 0
				MsgInfo("Processo finalizado, nenhum arquivo TXT foi criado!","Informação")
			else
				MsgInfo("Processo finalizado, "+str(nSucesso,12,0,.t.)+" TXT com sucesso e "+str(nFalha,12,0,.t.)+" TXT com falha!","Informação")
			endif
Function XLSToTxt(cFile, cHistorico, oText) 			
	local oExcel, oBook, oSheet, nLinha := 2, hLinha := {=>}, cLinha := ''
	local COLUNA_A, COLUNA_F, COLUNA_E
	local cArquivoTXT, cConteudoTXT := '', oFile
	try
      oExcel := CreateObject( "Excel.Application" )
      oBook := oExcel:WorkBooks:Open( cFile,;
       OleDefaultArg(), ;
       OleDefaultArg(), ;
       OleDefaultArg(), ;
       OleDefaultArg(), ;
       '1111')
      oSheet := oExcel:Get("ActiveSheet")
   catch oErro
      MsgAlert("Atenção não é possível abrir a planilha - erro Tecnico! - "+oErro:Description,"Alerta")
      return
   end
	*
   while .t.
      COLUNA_A       := oSheet:Cells(nLinha,1):Value // data do recebimento
      COLUNA_F       := oSheet:Cells(nLinha,6):Value // valor líquido da parcela
      COLUNA_E       := oSheet:Cells(nLinha,5):Value // valor MDR descontado 
      *
      if Empty(COLUNA_A)
         exit
      endif
      COLUNA_A := dtoc(COLUNA_A)
      oText:SetText("Data "+COLUNA_A)
      SysRefresh()
		*
      if HScan(hLinha, {|cKey| cKey == COLUNA_A }) > 0
      	hLinha[COLUNA_A][1] += COLUNA_F
      	hLinha[COLUNA_A][2] += COLUNA_E
		else
			HSet(hLinha, COLUNA_A, {COLUNA_F, COLUNA_E} )
		endif
		*
      nLinha++
	end
   *
	try
      oExcel:Quit()
   catch
   end
   Release oExcel
	*                              
	for each cLinha in hLinha:Keys
		 cConteudoTXT += cLinha+";"+alltrim(cHistorico)+";"+alltrim(transform(hLinha[cLinha][1], "@E 9999999999.99"))+";"+alltrim(transform(hLinha[cLinha][2], "@E 9999999999.99"))+CRLF
	next
	*
	if !Empty(cConteudoTXT)
	   cArquivoTXT:= cFilePath(cFile)+"\"+cFileNoExt(cFile)+".TXT"
	   delete file(cArquivoTXT)
	   oFile := TTxtFile():New(cArquivoTXT)
	   oFile:PutStr(cConteudoTXT)
	   oFile:End()
	   return .t.
 	endif
 	return .f.
	*
Function MsgAlertReturn(cMsg,cTitle,lReturn)
         MsgAlert(cMsg,cTitle)
         return lReturn
