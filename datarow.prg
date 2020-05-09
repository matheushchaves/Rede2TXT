/*
* FiveFastDialog.prg
* FWH 13.05
* Class TDataRow
*
*/
#include "fivewin.ch"
#include "dbinfo.ch"
#include "adodef.ch"
#include "xbrowse.ch"
#include "dbcombo.ch"
#include "Set.ch"
*
#define COLOR_BTNFACE       15
*
//----------------------------------------------------------------------------//
static lExit := .f.
//----------------------------------------------------------------------------//
CLASS TDataRow

   CLASSDATA cBmpList // Sample: "top=restop,up=<bmp>,down=<bmp>,bottom=<bmp>,new=<bmp>,redo=<bmp>,undo=<bmp>,save=,bmp>,close=<bmp>
                      // Case insensitive. Any order. Any number of bmps

   DATA aData
   DATA aOrg   //PROTECTED
   DATA aPrompts
   
   DATA aDefault
   DATA cTitle
   //
   DATA uSource
   DATA cSrcType
   //
	DATA oFocusInitialize
	DATA oBtnOK,oBtnCancel  ,oBtnRedo
   //
   ACCESS lValidData       INLINE ( ! Empty( ::cSrcType ) .and. ! Empty( ::aData ) )
   //
   //
   METHOD New( aData, oDbf ) CONSTRUCTOR
   //
   METHOD EsconderCampo( cn, lHide )       // --> lPrev
   MESSAGE Posicao        METHOD Posicao
   METHOD Posicao( u )
   METHOD Edit(lParametr1, lParametr2, cTitle, cMsg) INLINE ::ExibirDialog(lParametr1, lParametr2, cTitle, cMsg)
   METHOD ExibirDialog(lParametr1, lParametr2, cTitle, cMsg)
   METHOD Desfazer()
   METHOD Carregar()
   METHOD CriarControles( oPanel, oSayFont, oFixed,   cMsg ) 
   METHOD RealizarControle( hDC, oPanel, nRow, nAt,  oSayFont, oFixed, nSayWidth, nGetWidth ) 
	METHOD DlgButtons( oDlg, oPanel,   nGets ) 
	METHOD End()         INLINE ( ::uSource := nil, ::cSrcType := "", ::aData := nil )
   //
	DESTRUCTOR Destroy
   //

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( uSource ) CLASS TDataRow

   local n
	
   *
	::uSource   := uSource
	*
	::oFocusInitialize := NIL
   ::Carregar(  )

return Self

//----------------------------------------------------------------------------//

METHOD Carregar(  ) CLASS TDataRow
		
		local n, cKey
		local lReload  := ( ValType( ::aData ) == 'A' )
		
		::cSrcType     := "ARR"
		::aData        := ::uSource
		if ! Empty( ::aData ) .and. ValType( ::aData[ 1 ] ) == 'A' .and. Len( ::aData[ 1 ] ) > 1
		   ::aOrg      := AClone( ::aData )
		   AEval( ::aOrg, { |a| If( ValType( a[ 2 ] ) == 'B', a[ 2 ] := Eval( a[ 2 ] ), nil ) } )
		   for n := 1 to Len( ::aData )
		      if Len( ::aData[ n ] ) < 4
		         ASize( ::aData[ n ], 4 )
		         if ::aData[ n, 3 ] == nil
		            ::aData[ n, 3 ] := .t.
		         endif
		         if ::aData[ n, 4 ] == nil .and. ValType( ::aData[ n, 2 ] ) == 'N'
		            ::aData[ n, 4 ] := GetNumPict( ::aData[ n, 2 ] )
		         endif
		      endif
		   next
		endif
		   
			
		if ::lValidData
		   if ::aPrompts == nil
		      ::aPrompts     := {}
		      AEval( ::aData, { |a| AAdd( ::aPrompts, a[ 1 ] ) } )
		   endif
		   if ::aDefault == nil
		      ::aDefault     := Array( Len( ::aData ) )
		   else
		      if ::RecNo == 0
		         AEval( ::aDefault, { |u,i| If( u == nil .or. ValType( u ) == 'B', nil, ::aData[ i, 2 ] := u ) } )
		      endif
		   endif
		else
		   // MsgAlert( "Invalid Data" )
		endif

return ::lValidData
METHOD Posicao( cName ) CLASS TDataRow

   local nPos     := 0

   if ValType( cName ) == 'N'
      return cName
   endif
   cName    := Upper( AllTrim( cName ) )
   if ( nPos := AScan( ::aPrompts, { |c| Upper( c ) == cName } ) ) == 0
   if ( nPos := AScan( ::aPrompts, { |c| Upper( CharRem( ' ', c ) ) == cName } ) ) == 0
   if ( nPos := AScan( ::aPrompts, { |c| Upper( StrTran( c, ' ', '_' ) ) == cName } ) ) == 0
      nPos  := AScan( ::aData, { |a| Upper( Trim( a[ 1 ] ) ) == cName } )
   endif
   endif
   endif

return nPos

//----------------------------------------------------------------------------//

METHOD EsconderCampo( fld, lHide ) CLASS TDataRow

   local lRet := .f.
	local FLD_HIDE := 7
   if ( fld := ::Posicao( fld ) ) > 0
      if Len( ::aData[ fld ] ) >= FLD_HIDE
         lRet     := ( ::aData[ fld, FLD_HIDE ] == .t. )
      endif
      //
      if ValType( lHide ) == 'L'
         if Len( ::aData[ fld ] ) >= FLD_HIDE
            ::aData[ fld, FLD_HIDE ]   := lHide
         elseif lHide
            ASize( ::aData[ fld ], FLD_HIDE )
            ::aData[ fld, FLD_HIDE ]   := lHide
         endif
      endif
   endif

return lRet

METHOD Desfazer( fld ) CLASS TDataRow

   if PCount() > 0
      if ( fld := ::Posicao( fld ) ) > 0
         if ValType( ::aData[ fld, 2 ] ) == 'B'
            Eval( ::aData[ fld, 2 ], ::aOrg[ fld, 2 ] )
         else
            ::aData[ fld, 2 ] := ::aOrg[ fld, 2 ]
         endif
      endif
   else
      AEval( ::aData, { |a,i| If( ValType( a[ 2 ] ) == 'B', ;
                  Eval( a[ 2 ], ::aOrg[ i, 2 ] ), ;
                  a[ 2 ] := ::aOrg[ i, 2 ] ) } )
   endif

return nil

//----------------------------------------------------------------------------//

METHOD ExibirDialog( lParametr1, lParametr2, cTitle, cMsg ) CLASS TDataRow

   local oRec  := Self
   local oDlg, oPanel
   local oFont, oSayFont, oFixed
   local uRet

   if ! ::lValidData
      return .f.
   endif

   DEFAULT cTitle := ""

	*
   DEFINE FONT oFont NAME "TAHOMA" SIZE 0,-14
   oSayFont := oFont:Bold()
   DEFINE FONT oFixed  NAME "COURIER NEW" SIZE 0,-16
	*
   DEFINE DIALOG oDlg SIZE 800,500 PIXEL FONT oFont TITLE cTitle GRADIENT { { 1, GetSysColor( 15 ), GetSysColor( 15 ) } } 
   *
	oPanel   := TScrollPanel():New( 20, 20, 200, 360, oDlg, .t. )
   oPanel:SetColor( CLR_BLACK, oDlg:nClrPane )
   oPanel:SetFont( oDlg:oFont )
	*
   lExit    := .f.
   ACTIVATE DIALOG oDlg CENTERED ;
      ON INIT oRec:CriarControles( oPanel, oSayFont, oFixed, cMsg )  ;
      VALID ( !GetKeyState(27) )
   *
	RELEASE FONT oFont, oFixed, oSayFont
	*
return lExit 

//----------------------------------------------------------------------------//

METHOD CriarControles( oPanel, oSayFont, oFixed, cMsg ) CLASS TDataRow

   local nItem, oSay, oGet, nGets := 0
   local nRow  := 20
   local nSayWidth, nGetWidth, nWidth
   local nMaxSayWidth   := 0
   local nMaxGetWidth   := 0
   local oDlg     := oPanel:oWnd
   local hDC      := oDlg:GetDC()
   local nPanelWidth, nPanelHeight
	
	for nItem := 1 to Len( ::aData )
      if ! ::EsconderCampo( nItem )
         if LEFT(UPPER(::aPrompts[ nItem ]),4) # "SAY:"
				nMaxSayWidth   := Max( nMaxSayWidth, GetTextWidth( hDC, ::aPrompts[ nItem ] + " :", oSayFont:hFont ) )
      	endif
      endif
   next

   for nItem := 1 to Len( ::aData )
      if ! ::EsconderCampo( nItem )
         ::RealizarControle( hDC, oPanel, @nRow, nItem, oSayFont, oFixed, nMaxSayWidth, @nGetWidth )
         nMaxGetWidth   := Max( nMaxGetWidth, nGetWidth )
      endif
   next
   
	nGets    := 0
   for nItem := 2 to Len( oPanel:aControls ) STEP 2
      oGet     := oPanel:aControls[ nItem ]
      if oGet:ClassName() == "TMULTIGET"
         oGet:nWidth := nMaxSayWidth + 10 + nMaxGetWidth
      endif
      nGets++
   next
   oDlg:ReleaseDC()
   oPanel:SetRange()
   
	nPanelWidth    := Max( 200 , 20 + nMaxSayWidth + 10 + nMaxGetWidth + 20 + 24 )
   nPanelHeight   := Min(  Int( ScreenHeight() * 0.8 - 100 ), ;
	                           ATail( oPanel:aControls ):nBottom + 20 )
   
	oDlg:SetCoors(   TRect():new(  0,  0, 40 + nPanelHeight + 100, 40 + nPanelWidth + 40  ) )
   oPanel:SetCoors( TRect():New( 40, 40, 40 + nPanelHeight,      40 + nPanelWidth       ) )
   oDlg:bPainted  := { || oDlg:Box( oPanel:nTop - 2, oPanel:nLeft - 1, oPanel:nBottom + 2, oPanel:nRight + 2 ) }
   
	if nPanelHeight >= ATail( oPanel:aControls ):nBottom
      oPanel:nScrollRange     := 0
      oPanel:oVScroll:SetRange( 0, 0 )
      oPanel:WinStyle( WS_VSCROLL, .f. )
   endif
   ::DlgButtons( oDlg, oPanel,   nGets )
   
	if ! Empty( cMsg )
      @ 06,00 SAY cValToChar( cMsg ) SIZE oDlg:nWidth, 20 PIXEL OF oDlg CENTER TRANSPARENT
   endif
   
	oDlg:Center()

   if ::oFocusInitialize <> NIL
		xSetFocus(::oFocusInitialize)
	ENDIF	
	
	//oPanel:aControls[ 2 ]:SetFocus()
   
   SysRefresh()
   
return .f.

METHOD RealizarControle( hDC, oPanel, nRow, nAt, oSayFont, oFixed, nSayWidth, nGetWidth ) CLASS TDataRow

#define ROWHT  22
#define ROWGAP  4

   local oRec     := Self
   local aLine, oGet, bGet, bValid, bAction
   local lDate,lCheck, lCombo, lMemo, cType, cPic, cVal
   local lPassWord   := .f.
   local nCol     := 20 + nSayWidth + 10
   local cBMP:= ""
   local oSayPROMPT
	local lSayDescription := .f.
	Local aSay,bSay,eSay,cSay,nSayGetWidth,oSay
	
		if LEN(::aPrompts) >= nAt+1
			if upper(left(::aPrompts[ nAt+1 ],4)) == "SAY:"
				lSayDescription := .t.
			endif
		endif	

   aLine       := AClone( ::aData[ nAt ] )
   ASize( aLine, 5 )
   DEFAULT aLine[ 3 ] := .t.
	*
   if ValType( aLine[ 2 ] ) == 'B'
      bGet     := aLine[ 2 ]
      cType    := ValType( Eval( bGet ) )
   else
      cType       := ValType( IfNil( aLine[ 2 ], Space( 60 ) ) )
   endif
   *
	lDate       := cType == 'D'
   lCheck      := ( cType == 'L' )
   lCombo      := ValType( aLine[ 4 ] ) == 'A'
   lMemo       := .f.
	if ValType( aLine[ 4 ] ) == 'C' 
		lMemo := Upper( aLine[ 4 ] ) == 'M' 
	endif	
		
      if oRec:aData[ nAt, 2 ] == nil
         DEFAULT bGet := { |x| If( x == nil, IfNil( oRec:aData[ nAt, 2 ], Space( 60 ) ), ;
                           oRec:aData[ nAt, 2 ] := If( Empty( x ), nil, x ) ) }
      else
         DEFAULT bGet := bSETGET( oRec:aData[ nAt, 2 ] )
      endif
      bValid   := aLine[ 5 ]

   if ValType( aLine[ 4 ] ) == 'C' 
		if Len( aLine[ 4 ] ) > 1
      	cPic     := aLine[ 4 ]
      endif
   elseif ValType( Eval( bGet ) ) == 'N'
      cPic     := GetNumPict( Eval( bGet ) )
   endif
   if lMemo
      nGetWidth   := 360
   else
      if cType == 'C' .and. iif (cPic <> NIL, At("9",cPic) = 0,.T.)
         cVal     := Replicate( 'W', Len( Eval( bGet ) ) )
         lPassword:= ( iif(ValType( aLine[ 4 ] ) == 'L' , aLine[ 4 ] == .t.,.f. ) ) .or. ;
                     "PASSWORD" $ Upper( ::aPrompts[ nAt ] ) .or. ;
                     "PASSWORD" $ Upper( aLine[ 1 ] )
      else
         cVal     := Replicate( '9', Len( Transform( Eval( bGet ), cPic ) ) )
      endif
      if lCombo
         if ValType( aLine[ 4 ][ 1 ] ) == 'A'
            AEval( aLine[ 4 ], { |a| If( Len( a[ 2 ] ) > Len( cVal ), cVal := a[ 2 ], nil ) } )
         else
            AEval( aLine[ 4 ], { |c| If( Len( c ) > Len( cVal ), cVal := c, nil ) } )
         endif
         cVal     := Replicate( 'W', Len( cVal ) )
      endif
		if lSayDescription
			nGetWidth   := GetTextWidth( hDc, cVal, oPanel:oFont:hFont ) + 30
		else 
			nGetWidth   := GetTextWidth( hDc, cVal, oPanel:oFont:hFont ) + 20 + If( lCombo, 30, 0 )
			nGetWidth   := Min( 500, nGetWidth )
		endif
		
   endif
	*
	IF upper(left(::aPrompts[ nAt ],4)) == "SAY:"
		return nil
	ENDIF 
	*
   if lMemo
      @ nRow, 20 SAY oSayPROMPT PROMPT ::aPrompts[ nAt ] SIZE  nSayWidth,ROWHT PIXEL OF oPanel FONT oSayFont // TRANSPARENT
      oSayPROMPT:SetColor( CLR_BLACK, oPanel:nClrPane )
   else
      @ nRow, 20 SAY oSayPROMPT PROMPT ::aPrompts[ nAt ] + " :" SIZE  nSayWidth,ROWHT PIXEL OF oPanel FONT oSayFont RIGHT // TRANSPARENT
      oSayPROMPT:SetColor( CLR_BLACK, oPanel:nClrPane )
   endif

   if lMemo
      
		oGet := ;
      TMultiGet():New( nRow + 22 + 4, 20, bGet, oPanel, 500, 4 * 22 + 3 * 4, oFixed, ;
         .F.,nil,nil,nil, .T.,nil, .t.,nil, .F., .F., .f., bValid, nil, .F., nil, nil )
	
//   elseif lDate
//		/*
//		 TDatePick():New( <nRow>, <nCol>, bSETGET(<uVar>),;
//                                        [<oWnd>], <nWidth>, <nHeight>, <{ValidFunc}>,;
//                                        <nClrFore>, <nClrBack>, <oFont>, <.design.>,;
//                                        <oCursor>, <.pixel.>, <cMsg>, <.update.>, <{uWhen}>,;
//                                        [\{|Self| <uChange>\}], <nHelpId>, ;
//                                        <pic> )   
//		*/
//		oGet   := TDatePick():New( nRow, nCol, bGet, oPanel, nGetWidth,ROWHT,bValid, CLR_WHITE, CLR_BLACK, oPanel:oFont, .F., nil, .T., nil, .T., nil )
//		oGet:cVarName := aLine[1]
		if nAt == len(::aData)
			oGet:bKeyChar:={ |nKey, nFlags| IIf( nKey == VK_RETURN, XSETFOCUS(::oBtnOK) , ) }
		endif

   elseif lCheck
		
		if right(FWVERSION,5) == "17.09"
		   oGet   := ;
		    TSwitch():New( nRow + 2, nCol, "",;
		    bGet, oPanel, ROWHT * 1.5, ROWHT * 2 / 3, nil,;
		    nil, oPanel:oFont, bValid, CLR_GREEN, CLR_HGRAY,;
		    .f., .t., nil, .t., nil, ;
		    1, CLR_BLACK, 1, CLR_WHITE, .F., .t. )
		   oGet:lReadOnly    := .f.
		else
		   oGet   := ;
		    TCheckBox():new( nRow, nCol, "", bGet, oPanel, ROWHT,ROWHT, nil, ;
		    bValid, oPanel:oFont, nil, nil, nil, ;
		    .f., .t., nil, .t., nil )
			   
		endif
      oGet:cVarName := aLine[1]
		if nAt == len(::aData)
			oGet:bKeyChar:={ |nKey, nFlags| IIf( nKey == VK_RETURN, XSETFOCUS(::oBtnOK) , ) }
		endif
      
   elseif lCombo
      if ValType( aLine[ 4 ][ 1 ] ) == 'A'
         oGet := ;
         TDBCombo():New( nRow, nCol, bGet, nil, nGetWidth, If( IsAppThemed(), ROWHT, 300 ), oPanel, nil, ;
             bValid, nil, nil, nil,;
             .t., oPanel:oFont, nil, .t., nil,;
             .f., nil, 2, ;
             aLine[ 4 ], '1', '2', NIL ) //<aList> )
      else
         oGet   := ;
         TComboBox():New( nRow, nCol, bGet, aLine[ 4 ], nGetWidth, If( IsAppThemed(), ROWHT, 300 ), ;
            oPanel, nil, ;
            bValid, nil, nil, nil, ;
            .T., oPanel:oFont, nil, .T., nil, ;
            .F., nil, nil, nil, ;
            nil, nil )
      endif
      oGet:cVarName := aLine[1]
		if nAt == len(::aData)
			oGet:bKeyChar:={ |nKey, nFlags| IIf( nKey == VK_RETURN, XSETFOCUS(::oBtnOK) , ) }
		endif
   else
      if Left( ::aPrompts[ nAT ], 4 ) == "nClr" .or. ;
         Left( ::aData[ nAt, 1 ], 4 ) == "nClr"
         bAction   := { |o| BtnChooseColor( o ) }
      endif
      if lSayDescription
			if Valtype(::aData[ nAt + 1 ][5]) == "B"
				bAction := ::aData[ nAt + 1 ][5]
				cBMP    := StrTran(Alltrim(Upper(::aPrompts[ nAt + 1 ])),'SAY:')
			endif	
      endif
		oGet   := ;
		TGet():New( nRow, nCol, bGet, oPanel, nGetWidth,ROWHT, cPic, ;
               bValid, nil, nil, oPanel:oFont, .F., ;
               nil, .T., nil, .T., nil, ;
               .F., VALTYPE(EVAL(bGet)) $ "DNT", ;
               nil, .f., lPassword, .f., nil, nil, ;
               nil, aLine[1], nil, nil, bAction,cBMP )
      oGet:nClrTextDis  := CLR_BLACK
      oGet:nClrPaneDis  := GetSysColor( COLOR_BTNFACE )
      oGet:lDiscolors   := .f.
      oGet:cVarName := aLine[1]
		if lPassWord
         oGet:WinStyle( ES_PASSWORD, .t. )
      endif
		oGet:WinStyle( ES_AUTOHSCROLL, .t. )
		if iif(lSayDescription, nAt+1 == len(::aData), nAt == len(::aData))
			oGet:bKeyChar:={ |nKey, nFlags| IIf( nKey == VK_RETURN, (XSETFOCUS(::oBtnOK),) , ) }
		endif
		
		if lSayDescription
		   *
			aSay := ::aData[ nAt + 1 ]
			bSay := aSay[2]
			eSay := eval(aSay[2])
			*
			cSay     := Replicate( 'W', Len( Eval( bSay ) ) )
			nSayGetWidth   := oPanel:nWidth
			nSayGetWidth   := Min( 500, nSayGetWidth )
			*
			oSay := TSay():New( nRow, nGetWidth + nCol + 5, bSay,;
             oPanel, nil, oSayFont, .f., .f., .f.,;
             .t., nil, nil, nSayGetWidth,ROWHT,;
             .f., .t., .f., .f., .f., .t.,;
             .t., aSay[1] )
			*
			oGet:Cargo := oSay
			*
		endif 
		*
   endif
	if ::oFocusInitialize == NIL .and. oGet <> NIL
	   ::oFocusInitialize := oGet
	endif   

  	nRow     += If( lMemo, 5, 1 ) * ( ROWHT + ROWGAP )

return nil

//----------------------------------------------------------------------------//

METHOD DlgButtons( oDlg, oPanel,  nGets ) CLASS TDataRow
	
	local oRec           := Self
	local oFont
	local nRow, nCol, oBtn
	local lCanNavigate   := .F.
	local lDataSource    := .F.
	local aBmp           := MakeBmpArray( ::cBmpList )
	
	nRow        := oPanel:nBottom + ROWHT
	nCol        := oPanel:nRight - 32
	*
	aBmp[ 1, 3 ]   :=  Chr(0xFC)
	*
	oFont := TFont():New( "Wingdings", 0, -22, .f., .f., 0, 0, 400, .f., .f., .f., 2,3, 2, 1,, 18 )
	@ nRow, nCol BTNBMP ::oBtnOK FILE aBmp[ 1, 1 ] RESOURCE aBmp[ 1, 2 ] PROMPT aBmp[ 1, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
	 TOOLTIP "Confirmar" ACTION ( lExit := .t., oDlg:End() )
	::oBtnOK:SetColor( CLR_GREEN, ::oBtnOK:nClrPane )

	nCol  -= 36
	@ nRow, nCol BTNBMP ::oBtnCancel PROMPT Chr( 0xFB ) SIZE 32,32 PIXEL OF oDlg FONT oFont ;
	 TOOLTIP "Cancelar" ACTION ( oRec:Desfazer(), lExit := .f., oDlg:End() )
	::oBtnCancel:SetColor( CLR_HRED, ::oBtnCancel:nClrPane )
	::oBtnCancel:lCancel   := .t.
	
	RELEASE FONT oFont
	oFont := TFont():New( "Wingdings 3", 0, -22, .f., .f., 0, 0, 400, .f., .f., .f., 2,3, 2, 1,, 18 )
	nCol  -= 36
	
	@ nRow, nCol BTNBMP ::oBtnRedo FILE aBmp[ 3, 1 ] RESOURCE aBmp[ 3, 2 ] PROMPT aBmp[ 3, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
	 TOOLTIP "Desfazer" ACTION ( oRec:Desfazer(), oPanel:Update(), oPanel:SetFocus() ) WHEN nGets < 2
	::oBtnRedo:lCancel   := .t.
	RELEASE FONT oFont
	

return nil

//----------------------------------------------------------------------------//

PROCEDURE Destroy CLASS TDataRow

   ::End()

return


static function GetNumPict( n ); return NumPict( 11, GetDec( n ) )

static function GetDec( n )

   local nDec  := 0
   local c     := cValToChar( n )

   if '.' $ c
      nDec  := Len( AfterAtNum( '.', c ) )
   endif

return nDec
Function EditVarsGetValid(oGetBlock,oGet,oSay)
   Default oGet:=NIL
   Default oSay:=NIL
   oGet:=oGetBlock
   oSay:=oGetBlock:Cargo
   return .t.

Function EditVarsFindByVar(oIn,oOut,cVarName)
   Local aControls := {},oControl
	IF Valtype(oIn) <> "O"
      MsgStop("oIn não é um objeto","Erro EditVarsFindByVar")
      RETURN .f.
	ENDIF
	*
	if Empty(cVarName)
		MsgStop("cVarName deve conter informações","Erro EditVarsFindByVar")
		return .f.
	endif
	*
	aControls := oIn:oWnd:aControls
	for each oControl in aControls
		 if oControl:cVarName == cVarName
			 oOut:=oControl
			 exit
 		 endif
	next
	*
	*FWDBG oIn,oOut,cVarName
   return .t.

FUNCTION xSetFocus( oObj )
   *========================================================
   LOCAL _oWnd := oObj:oWnd, _oTempo := ""

   DEFINE Timer _oTempo Interval 10 of _oWnd ;
    Action ( oObj:SetFocus(), _oTempo:End() )

   ACTIVATE Timer _oTempo

   RETURN .T.
