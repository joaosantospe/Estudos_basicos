#INCLUDE "RWMAKE.CH"
                                                                                               
/*//alterado aqui
�����������������������������������������������������������������������������                                                          
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  � WFATR001 � Autor � Jo�o Paulo Santos     � Data � 19/10/20 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � IMPRESSAO DO BOLETO BANCO DO BRASIL COM CODIGO DE BARRAS   ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Cliente Microsiga Wings                    ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/
User Function SMBOLBB()

LOCAL	aPergs := {}
LOCAL   aMarked    := {}
PRIVATE lExec    := .F.
PRIVATE cIndexName := ''
PRIVATE cIndexKey  := ''
PRIVATE cFilter    := ''
PRIVATE cEhJur

Tamanho  := "M"
titulo   := "Impressao de Boleto com Codigo de Barras"
cDesc1   := "Este programa destina-se a impressao do Boleto BB com Codigo de Barras."
cDesc2   := ""
cDesc3   := ""
cString  := "SE1"
wnrel    := "BOLETO BANCO DO BRASIL"
lEnd     := .F.
cPerg     :="BOLSBB"
aReturn  := {"Zebrado", 1,"Administracao", 2, 2, 1, "",1 }
nLastKey := 0
                            
dbSelectArea("SE1")
	ValidPerg()
		Pergunte (cPerg,.F.)
			Wnrel := SetPrint(cString,Wnrel,cPerg,@Titulo,cDesc1,cDesc2,cDesc3,.F.,,,Tamanho,,)

If nLastKey == 27
	Set Filter to
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
	Set Filter to
	Return
Endif

cIndexName := Criatrab(Nil,.F.)


cIndexKey   := 	"E1_PORTADO+E1_CLIENTE+E1_PREFIXO+E1_NUM+E1_PARCELA+E1_TIPO+DTOS(E1_EMISSAO)"
cFilter    	  := 	"E1_PREFIXO >= '" + MV_PAR01 + "' .And. E1_PREFIXO <= '" + MV_PAR02 + "' .And. " + ;
"E1_NUM     >= '" + MV_PAR03 + "' .And. E1_NUM     <= '" + MV_PAR04 + "' .And. " + ;
"E1_PARCELA >= '" + MV_PAR05 + "' .And. E1_PARCELA <= '" + MV_PAR06 + "' .And. " + ;
"E1_NUMBOR >= '" + MV_PAR07 + "' .And. E1_NUMBOR <= '" + MV_PAR08 + "'"// .And. E1_NUMBOR <> '' "


//"SubsTring(E1_TIPO,3,1) != '-' .And. "+;
//"E1_PORTADO != '   '"
IndRegua("SE1", cIndexName, cIndexKey,, cFilter, "Aguarde selecionando registros....")

//IndRegua("SE1", cIndexName, cIndexKey,, cFilter, "Aguarde selecionando registros....")
DbSelectArea("SE1")
#IFNDEF TOP
	DbSetIndex(cIndexName + OrdBagExt())
#ENDIF
dbGoTop()
@ 001,001 TO 400,700 DIALOG oDlg TITLE "Sele��o de Titulos"
@ 001,001 TO 170,350 BROWSE "SE1" MARK "E1_OK"
@ 180,310 BMPBUTTON TYPE 01 ACTION (lExec := .T.,Close(oDlg))
@ 180,280 BMPBUTTON TYPE 02 ACTION (lExec := .F.,Close(oDlg))
ACTIVATE DIALOG oDlg CENTERED

dbGoTop()

Do While !Eof()
	If Marked("E1_OK")
		cEhJur:= SMPESSOA(SE1->E1_CLIENTE,SE1->E1_LOJA)
			if cEhJur == "J"
				AADD(aMarked,.T.)
			else 
				Alert("Cliente nao e Juridico")
			EndIF		
	Else
		AADD(aMarked,.F.)
	EndIf
	dbSkip()
EndDo

If lExec
	Processa({|lEnd|MontaRel(aMarked)})
Endif

RetIndex("SE1")
Ferase(cIndexName+OrdBagExt())
Return Nil

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  �  MontaRel� Autor � Microsiga             � Data � 13/10/03 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � IMPRESSAO DO BOLETO LASER DO ITAU COM CODIGO DE BARRAS     ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Microsiga                         ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/
Static Function MontaRel(aMarked)
LOCAL cBoldias     :=SUPERGETMV ("SM_BOLDIAS",.T.,"5")
LOCAL oPrint
LOCAL nX           := 0
Local cNroDoc      :=  " "
Local cConvenio    := ""
LOCAL aDadosEmp    := {	SM0->M0_NOMECOM                                    ,; //[1]Nome da Empresa
                        SM0->M0_ENDCOB                                     ,; //[2]Endere�o
AllTrim(SM0->M0_BAIRCOB)+", "+AllTrim(SM0->M0_CIDCOB)+", "+SM0->M0_ESTCOB  ,; //[3]Complemento
"CEP: "+Subs(SM0->M0_CEPCOB,1,5)+"-"+Subs(SM0->M0_CEPCOB,6,3)              ,; //[4]CEP
"PABX/FAX: "+SM0->M0_TEL                                                   ,; //[5]Telefones
"CNPJ: "+Subs(SM0->M0_CGC,1,2)+"."+Subs(SM0->M0_CGC,3,3)+"."+               ; //[6]
Subs(SM0->M0_CGC,6,3)+"/"+Subs(SM0->M0_CGC,9,4)+"-"+                        ; //[6]
Subs(SM0->M0_CGC,13,2)                                                     ,; //[6]CGC
"I.E.: "+Subs(SM0->M0_INSC,1,3)+"."+Subs(SM0->M0_INSC,4,3)+"."+             ; //[7]
Subs(SM0->M0_INSC,7,3)+"."+Subs(SM0->M0_INSC,10,3)                         }  //[7]I.E

LOCAL aDadosTit
LOCAL aDadosBanco
LOCAL aDatSacado
LOCAL aBolText     := {"Ap�s o vencimento cobrar multa de R$ ",;
"Ap�s vencimento, Mora Diaria de R$ ",;
"Sujeito a Protesto apos 5 dias do vencimento"}

LOCAL nI            := 1
LOCAL i             := 1
LOCAL aCB_RN_NN     := {}
LOCAL nVlrAbat		:= 0
LOCAL cCond			:= ""
LOCAL nRec          := 0
Local cPortad       := ""
Local cAgenc        := ""
Local cContad       := ""
aArea := GetArea()
oPrint:= TMSPrinter():New( "Boleto Laser" )
//oPrinter:Setup()
oPrint:SetPortrait() //oPrint:SetLandscape()
oPrint:StartPage()   // Inicia uma nova p�gina

dbGoTop()
Do While !EOF()
	nRec := nRec + 1
	dbSkip()
EndDo

dbGoTop()

ProcRegua(nRec)
//POSICIONA NO CADASTRO DE CLIENTE E INCREMENTA NO TITULO OS CAMPOS PORTADOR AGENCIA E CONTA
DbSelectArea("SA1")
DbSetOrder(1)
cPortad := POSICIONE("SA1", 1, xFilial()+SE1->E1_CLIENTE+SE1->E1_LOJA, "A1_BCO1")

DBSELECTAREA("SA6")
DbSetOrder(1)
cAgenc := POSICIONE("SA6", 1, xFilial("SA6")+cPortad, "A6_AGENCIA")
cContad := POSICIONE("SA6", 1, xFilial("SA6")+cPortad, "A6_NUMCON")

//incluido por Sergio(SEMAR) em 10/01/2018 - temporario
cPortad:='001' 
cAgenc := '0007 ' 

If cFilAnt =="01"  //Eduardo(Semar) em 30/01/2018 -Correcao Filial
cContad:= '32953     '
End 

//Posiciona o SA6 (Bancos)
DbSelectArea("SA6")
DbSetOrder(1)
//If !DbSeek(xFilial("SA6")+SE1->(E1_PORTADO+E1_AGEDEP+E1_CONTA))
If !DbSeek(xFilial("SA6")+cPortad+cAgenc+cContad)
	//	return nil
	MsgStop("O titulo "+Trim(SE1->(E1_PREFIXO+E1_NUM+E1_PARCELA))+" n�o ser� impresso, pois o banco n�o esta cadastrado!","Aten��o")
	//   return
EndIf

Do While !SE1->(EOF())
	
	//Posiciona o SA1 (Cliente)
	DbSelectArea("SA1")
	DbSetOrder(1)
	//DbSeek(xFilial()+SE1->E1_CLIENTE+SE1->E1_LOJA,.T.)
	If !DbSeek(xFilial()+SE1->E1_CLIENTE+SE1->E1_LOJA)
		DBSkip()
		Loop
		MsgStop("O titulo "+Trim(SE1->(E1_PREFIXO+E1_NUM+E1_PARCELA))+" n�o ser� impresso, pois o cliente n�o esta cadastrado!","Aten��o")
	EndIf
	
	DbSelectArea("SE1")
	
	If aMarked[i]
		DbSelectArea("SEE")
		DbSetOrder(1)
		DbSeek(xFilial("SEE")+cPortad+cAgenc+cContad,.T.)
		If Empty(SE1->E1_NUMBCO)
			//Posiciona na Arq de Parametros CNAB
			cNroDoc := StrZero(Val(Alltrim(SEE->EE_FAXATU))+1,7) 
			RecLock("SEE",.f.)
			//RECLOCK("TMPNA",.T.)
			SEE->EE_FAXATU :=	cNroDoc
			
			MsUnlock()
			RecLock("SE1",.f.)
			//RECLOCK("TMPNA",.T.)
			SE1->E1_NUMBCO :=	cNroDoc
			MsUnlock()
			
		Else
			cNroDoc := Substr(SE1->E1_NUMBCO,1,8)
		Endif
		
		cConvenio := SEE->EE_CODEMP
	Endif
	
	
	aDadosBanco  := {SA6->A6_COD,;								// [1]Numero do Banco
	SA6->A6_NOME,;												// [2]Nome do Banco
	alltrim(SA6->A6_AGENCIA)		,;							// [3]Ag�ncia
	alltrim(SA6->A6_NUMCON)		,;							 	// [4]Conta Corrente
	Alltrim(SA6->A6_DVCTA)		,;						 		// [5]D�gito da conta corrente
	SEE->EE_SUBCTA,;											// [6]Codigo da Carteira
	Alltrim(SA6->A6_DVAGE)         }                            // [7]Digito verificado da ag�ncia 
	
	If Empty(SA1->A1_ENDCOB)
		aDatSacado   := {AllTrim(SA1->A1_NOME),;				// [1]Raz�o Social
		AllTrim(SA1->A1_COD )+"-"+SA1->A1_LOJA,;				// [2]C�digo
		AllTrim(SA1->A1_END )+"-"+AllTrim(SA1->A1_BAIRRO),;  	// [3]Endere�o
		AllTrim(SA1->A1_MUN ),;									// [4]Cidade
		SA1->A1_EST,;											// [5]Estado
		SA1->A1_CEP,;											// [6]CEP
		SA1->A1_CGC,;											// [7]CGC
		SA1->A1_PESSOA}											// [8]PESSOA
	Else
		aDatSacado   := {AllTrim(SA1->A1_NOME),;				// [1]Raz�o Social
		AllTrim(SA1->A1_COD )+"-"+SA1->A1_LOJA,;				// [2]C�digo
		AllTrim(SA1->A1_ENDCOB)+"-"+AllTrim(SA1->A1_BAIRROC),;	// [3]Endere�o
		AllTrim(SA1->A1_MUNC),;									// [4]Cidade
		SA1->A1_ESTC,;											// [5]Estado
		SA1->A1_CEPC,;											// [6]CEP
		SA1->A1_CGC,;											// [7]CGC
		SA1->A1_PESSOA}											// [8]PESSOA
	Endif
	
	nVlrAbat   :=  SomaAbat(SE1->E1_PREFIXO,SE1->E1_NUM,SE1->E1_PARCELA,"R",1,,SE1->E1_CLIENTE,SE1->E1_LOJA)
	
	//Aqui defino parte do nosso numero. Sao 8 digitos para identificar o titulo.
	//Abaixo apenas uma sugestao
	
	//Monta codigo de barras   

    // Chama a fun��o RET_CBARRAS PASSANDO POR REFERENCIA OS DADOS BANCRIOS ABAIXO:
    //                        [1]Numero do Banco       [3]Ag�ncia     [4]Conta CC    [5]D�gitocc  E1_NUMBCO               CALCULO        E1_VENCTO EE_CODEMP
	aCB_RN_NN  := Ret_cBarra(Subs(aDadosBanco[1],1,3),aDadosBanco[3],aDadosBanco[4],aDadosBanco[5],cNroDoc,(SE1->E1_VALOR-nVlrAbat),SE1->E1_VENCTO,cConvenio)                            
	                                  	       
	aDadosTit	:= {AllTrim(SE1->E1_NUM)+AllTrim(SE1->E1_PARCELA),;	   // [1] N�mero do t�tulo
	SE1->E1_EMISSAO,;				             		               // [2] Data da emiss�o do t�tulo
	dDataBase,;							     						   // [3] Data da emiss�o do boleto
	SE1->E1_VENCTO,;							                       // [4] Data do vencimento
	(SE1->E1_SALDO - nVlrAbat),;				                       // [5] Valor do t�tulo	
    aCB_RN_NN[3],;						                               // [6] Nosso n�mero (Ver f�rmula para calculo)
	SE1->E1_PREFIXO,;						                           // [7] Prefixo da NF
	SE1->E1_TIPO}							                           // [8] Tipo do Titulo
	
	If Marked("E1_OK")
		Impress(oPrint,aDadosEmp,aDadosTit,aDadosBanco,aDatSacado,aBolText,aCB_RN_NN,cConvenio)
		nX := nX + 1
	EndIf
	
	SE1->(dbSkip())
	IncProc()                                                                       
	
	nI := nI + 1
	i := i + 1
	
EndDo
RestArea(aArea)
	oPrint:EndPage()     // Finaliza a p�gina
		oPrint:Preview()     // Visualiza antes de imprimir

Return nil

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  �  Impress � Autor � Microsiga             � Data � 13/10/03 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � IMPRESSAO DO BOLETO LASERDO ITAU COM CODIGO DE BARRAS      ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Microsiga                         ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/
Static Function Impress(oPrint,aDadosEmp,aDadosTit,aDadosBanco,aDatSacado,aBolText,aCB_RN_NN,cConvenio)
LOCAL oFont8
LOCAL oFont11c
LOCAL oFont10
LOCAL oFont14
LOCAL oFont16n
LOCAL oFont15
LOCAL oFont14n
LOCAL oFont24
LOCAL nI := 0


//Parametros de TFont.New()
//1.Nome da Fonte (Windows)
//3.Tamanho em Pixels
//5.Bold (T/F)
oFont8  := TFont():New("Arial",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
oFont11c := TFont():New("Courier New",9,11,.T.,.T.,5,.T.,5,.T.,.F.)
oFont10  := TFont():New("Arial",9,09,.T.,.T.,5,.T.,5,.T.,.F.)
oFont14  := TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
oFont20  := TFont():New("Arial",9,20,.T.,.T.,5,.T.,5,.T.,.F.)
oFont21  := TFont():New("Arial",9,21,.T.,.T.,5,.T.,5,.T.,.F.)
oFont16n := TFont():New("Arial",9,16,.T.,.F.,5,.T.,5,.T.,.F.)
oFont15  := TFont():New("Arial",9,15,.T.,.T.,5,.T.,5,.T.,.F.)
oFont15n := TFont():New("Arial",9,15,.T.,.F.,5,.T.,5,.T.,.F.)
oFont14n := TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
oFont24  := TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)

oPrint:StartPage()   // Inicia uma nova p�gina

/******************/
/* PRIMEIRA PARTE */
/******************/

nRow1 := -600

oPrint:Line (nRow1+0710,100,nRow1+0710,2300)
oPrint:Line (nRow1+0710,500,nRow1+0630, 500)
oPrint:Line (nRow1+0710,710,nRow1+0630, 710)

//oPrint:SayBitmap(nRow1+0644,100,"santander_logo.bmp",10,60,,.t.)
oPrint:SayBitmap(nRow1+0644,100,"brasil_logo.bmp",200,60,,.t.)
oPrint:Say  (nRow1+0635,513,aDadosBanco[1]+"-7",oFont21 )	// [1]Numero do Banco
oPrint:Say  (nRow1+0644,1800,"Recibo do Pagador",oFont10)

oPrint:Line (nRow1+0810,100,nRow1+0810,2300 )
oPrint:Line (nRow1+0910,110,nRow1+0910,2300 )
oPrint:Line (nRow1+1000,110,nRow1+1000,2300 )

oPrint:Line (nRow1+0910,1000,nRow1+0980,1000)
oPrint:Line (nRow1+0910,1400,nRow1+0980,1400)
oPrint:Line (nRow1+0910,1800,nRow1+0980,1800)

oPrint:Say  (nRow1+0710,100 ,"",oFont8)
oPrint:Say  (nRow1+0725,400 ,"",oFont10)
oPrint:Say  (nRow1+0765,400 ,"",oFont10)

oPrint:Say  (nRow1+0710,1810,""                                                           ,oFont8)
cString	:= ""
nCol := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow1+0750,nCol,cString,oFont11c)

oPrint:Say  (nRow1+0810,100 ,"Benefici�rio"                                               ,oFont8)
oPrint:Say  (nRow1+0840,100 ,alltrim(aDadosEmp[1])+" - "+aDadosEmp[6]	                  ,oFont10) //Nome + CNPJ
oPrint:Say  (nRow1+0874,100 ,alltrim(aDadosEmp[2])+" - "+aDadosEmp[3]+" - "+aDadosEmp[4]  ,oFont8)  //Endere�o +  Complemento  + Cep

oPrint:Say  (nRow1+0810,1810,"Vencimento",oFont8)
cString := StrZero(Day(aDadosTit[4]),2) +"/"+ StrZero(Month(aDadosTit[4]),2) +"/"+ Right(Str(Year(aDadosTit[4])),4)
nCol := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow1+0850,nCol,cString,oFont11c)

oPrint:Say  (nRow1+0910,100 ,"Pagador"                             	  					                     	  ,oFont8)
oPrint:Say  (nRow1+0940,100 ,aDatSacado[1]                                   		                    		  ,oFont10)      //Nome  
oPrint:Say  (nRow1+0968,100 ,aDatSacado[3]+" - "+aDatSacado[4]+" - "+aDatSacado[5]+" - "+aDatSacado[6]			  ,oFont8)       //Endere�o+Cidade+Estado+CEP  
//Editado por Jo�o Paulo Santos em 21/10/2020
//oPrint:Say  (nRow1+0940,100, StrZero(Day(aDadosTit[2]),2) +"/"+ StrZero(Month(aDadosTit[2]),2) +"/"+ Right(Str(Year(aDadosTit[2])),4),oFont10)

oPrint:Say  (nRow1+0910,1010 ,"Nro.Documento"     	   										                      ,oFont8)
oPrint:Say  (nRow1+0940,1110,aDadosTit[7]+aDadosTit[1]															  ,oFont10) //Prefixo +Numero+Parcela

oPrint:Say  (nRow1+0910,1410,"Nosso N�mero"                                   									  ,oFont8)
// original: cString := substr(aDadosTit[6],1,12)+"-"+substr(aDadosTit[6],13,1)
cString :=  aDadosTit[6]
oPrint:Say  (nRow1+0940,1510,cString												   		 				      ,oFont10) //Tipo do Titulo

oPrint:Say  (nRow1+0910,1810,"Valor do Documento"                                             					  ,oFont8)
cString := Alltrim(Transform(aDadosTit[5],"@E 99,999,999.99"))
nCol := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow1+0940,nCol,cString,oFont11c)
oPrint:Say  (nRow1+1000,100 ,"Instru��es (Todas informa��es deste bloqueto s�o de exclusiva responsabilidade do benefici�rio.)",oFont8)


//auterado por Jo�o Paulo em 21/10/2020
//oPrint:SayBitmap(nRow1+1050,500,"brasil_logo.bmp")
oPrint:Say  (nRow1+1150,100 ,"Juros de Mora de 1% ao m�s.",oFont10)
oPrint:Say  (nRow1+1200,100 ,"Multa de 9% ap�s 5 dias de Vencimento.",oFont10) //comentado por joao paulo em 24/03/2020 por rordem de s�rgio
//oPrint:Say  (nRow1+1150,100 ,aBolText[1]+" "+AllTrim(Transform((aDadosTit[5]*0.01),"@E 99,999.99"))       ,oFont10)
//oPrint:Say  (nRow1+1200,100 ,aBolText[2]+" "+AllTrim(Transform((aDadosTit[5]*(SE1->E1_PORCJUR/100)),"@E 99,999.99"))  ,oFont10)
//oPrint:Say  (nRow1+1250,100 ,aBolText[3]                                        ,oFont10)
nRow1 += 200
oPrint:Line (nRow1+1500,100 ,nRow1+1500,2300 )
oPrint:Say  (nRow1+1560,100 ,"Recebimento atrav�s de cheque nr.__________________ do Banco ______________________________________"      ,oFont10)
oPrint:Say  (nRow1+1610,100 ,"Esta quitac�o s� ter� validade ap�s o pagamento do cheque"             ,oFont10)
oPrint:Say  (nRow1+1660,100 ,"pelo pagador",oFont10)

oPrint:Say  (nRow1+1730,100 ,"Pagador/Avalista",oFont10)
oPrint:Say  (nRow1+1730,1500,"CNPJ",oFont10)
oPrint:Line (nRow1+1770,100 ,nRow1+1770,2300 )




/*****************/
/* SEGUNDA PARTE */
/*****************/

nRow2 := 900


//Pontilhado separador
For nI := 100 to 2300 step 50
	oPrint:Line(nRow2+0580, nI,nRow2+0580, nI+30)
Next nI

nRow2 := nRow2+30

oPrint:Line (nRow2+0710,100,nRow2+0710,2300)
oPrint:Line (nRow2+0710,500,nRow2+0630, 500)
oPrint:Line (nRow2+0710,710,nRow2+0630, 710)

//oPrint:SayBitmap(nRow2+0644,100,"santander_logo.bmp",10,60,,.t.)
//comentado por jo�o dia 21/10/20
oPrint:SayBitmap(nRow1+1910,100,"brasil_logo.bmp",300,100,,.t.)
oPrint:Say  (nRow2+0635,0513,aDadosBanco[1]+"-7",oFont21 )	// [1]Numero do Banco
oPrint:Say  (nRow2+0635,0800,aCB_RN_NN[2],oFont15n)			//

oPrint:Line (nRow2+0810,100,nRow2+0810,2300 )
oPrint:Line (nRow2+0910,100,nRow2+0910,2300 )
oPrint:Line (nRow2+1010,100,nRow2+1010,2300 )
oPrint:Line (nRow2+1120,100,nRow2+1120,2300 )

//oPrint:Line (nRow2+0910,500,nRow2+1110,500)
oPrint:Line (nRow2+1010,750,nRow2+1110,750)
oPrint:Line (nRow2+0910,1000,nRow2+1110,1000)
oPrint:Line (nRow2+0910,1300,nRow2+1010,1300)
oPrint:Line (nRow2+0910,1480,nRow2+1110,1480)

oPrint:Say  (nRow2+0710,100 ,"Local de Pagamento",oFont8)
oPrint:Say  (nRow2+0750,300 ,"Pag�vel em qualque banco",oFont10)

oPrint:Say  (nRow2+0710,1810,"Vencimento"                                     ,oFont8)
cString	:= StrZero(Day(aDadosTit[4]),2) +"/"+ StrZero(Month(aDadosTit[4]),2) +"/"+ Right(Str(Year(aDadosTit[4])),4)
nCol := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow2+0750,nCol,cString,oFont11c)

oPrint:Say  (nRow2+0810,100 ,"Benefici�rio"                                        ,oFont8)
oPrint:Say  (nRow2+0850,100 ,alltrim(aDadosEmp[1])+" - "+aDadosEmp[6]	,oFont10) //Nome + CNPJ

oPrint:Say  (nRow2+0810,1810,"Ag�ncia/C�digo do benefici�rio",oFont8)
cString := aDadosBanco[3]+"-"+aDadosBanco[7]+"/"+aDadosBanco[4]+"-"+aDadosBanco[5]//Alltrim(AllTrim(aDadosBanco[3])+"/"+Trim(SUBSTR(cConvenio,1,7)))
nCol := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow2+0850,1820,cString,oFont11c)

oPrint:Say  (nRow2+0910,100 ,"Data do Documento"                              ,oFont8)
oPrint:Say  (nRow2+0940,100, StrZero(Day(aDadosTit[2]),2) +"/"+ StrZero(Month(aDadosTit[2]),2) +"/"+ Right(Str(Year(aDadosTit[2])),4),oFont10)

oPrint:Say  (nRow2+0910,505 ,"Nro.Documento"                                  ,oFont8)
oPrint:Say  (nRow2+0940,605 ,aDadosTit[7]+aDadosTit[1]					      ,oFont10) //Prefixo +Numero+Parcela

oPrint:Say  (nRow2+0910,1005,"Esp�cie Doc."                                   ,oFont8)
oPrint:Say  (nRow2+0940,1050,"DM"										      ,oFont10) //Tipo do Titulo

oPrint:Say  (nRow2+0910,1305,"Aceite"                                         ,oFont8)
oPrint:Say  (nRow2+0940,1400,"N"                                              ,oFont10)

oPrint:Say  (nRow2+0910,1485,"Data do Processamento"                           ,oFont8)
oPrint:Say  (nRow2+0940,1550,StrZero(Day(aDadosTit[3]),2) +"/"+ StrZero(Month(aDadosTit[3]),2) +"/"+ Right(Str(Year(aDadosTit[3])),4),oFont10) // Data impressao

oPrint:Say  (nRow2+0910,1810,"Nosso N�mero"                                    ,oFont8)
//criar parametro para inserir convenio conforme as boa praticas.

//original: cString := substr(aDadosTit[6],1,12)+"-"+substr(aDadosTit[6],13,1)
cString   := aDadosTit[6]
nCol      := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow2+0940,nCol,cString,oFont11c)

oPrint:Say  (nRow2+1010,100 ,"Carteira"                                         ,oFont8)
oPrint:Say  (nRow2+1033,100 ,"COBRANCA SIMPLES ECR"	                            ,oFont10)

//oPrint:Say  (nRow2+1010,505 ,"Carteira"                                       ,oFont8)
//oPrint:Say  (nRow2+1030,555 ,aDadosBanco[6]                                  	,oFont10)

oPrint:Say  (nRow2+1010,755 ,"Esp�cie"                                          ,oFont8)
oPrint:Say  (nRow2+1033,805 ,"REAL"                                             ,oFont10)

oPrint:Say  (nRow2+1010,1005,"Quantidade"                                       ,oFont8)
oPrint:Say  (nRow2+1010,1485,"Valor"                                            ,oFont8)

oPrint:Say  (nRow2+1010,1810,"Valor do Documento"                          	    ,oFont8)
cString := Alltrim(Transform(aDadosTit[5],"@E 99,999,999.99"))
nCol := 1810+(374-(len(cString)*22))
oPrint:Say  (nRow2+1030,nCol,cString ,oFont11c)

oPrint:Say  (nRow2+1110,100 ,"Instru��es (Todas informa��es deste bloqueto s�o de exclusiva responsabilidade do benefici�rio)",oFont8)
oPrint:Say  (nRow2+1150,100 ,"Juros de Mora de 1% ao m�s.",oFont10)
oPrint:Say  (nRow2+1200,100 ,"Multa de 9% ap�s 5 dias de Vencimento.",oFont10) //comentado por joao paulo em 24/03/2020 por rordem de s�rgio
//oPrint:Say  (nRow2+1150,100 ,aBolText[1]+" "+AllTrim(Transform((aDadosTit[5]*0.01),"@E 99,999.99"))       ,oFont10)
//oPrint:Say  (nRow2+1200,100 ,aBolText[2]+" "+AllTrim(Transform((aDadosTit[5]*(SE1->E1_PORCJUR/100)),"@E 99,999.99"))  ,oFont10)
//oPrint:Say  (nRow2+1250,100 ,aBolText[3]                                        ,oFont10)

oPrint:Say  (nRow2+1110,1810,"(-)Desconto"                                      ,oFont8)
oPrint:Say  (nRow2+1210,1810,"(-)Abatimento"                                    ,oFont8)
oPrint:Say  (nRow2+1310,1810,"(+)Mora"                                          ,oFont8)
oPrint:Say  (nRow2+1410,1810,"(+)Outros Acr�scimos"                             ,oFont8)
oPrint:Say  (nRow2+1510,1810,"(=)Valor Cobrado"                                 ,oFont8)

oPrint:Say  (nRow2+1610,100 ,"Pagador"                                          ,oFont8)
oPrint:Say  (nRow2+1620,400 ,aDatSacado[1]+" ("+aDatSacado[2]+")"               ,oFont10)
oPrint:Say  (nRow2+1660,400 ,aDatSacado[3]                                      ,oFont10)
oPrint:Say  (nRow2+1690,400 ,aDatSacado[6]+"    "+aDatSacado[4]+" - "+aDatSacado[5],oFont10) // CEP+Cidade+Estado


oPrint:Say  (nRow2+1710,100 ,"Pagador/Avalista",oFont8)
oPrint:Say  (nRow2+1750,1500,"Autentica��o Mec�nica",oFont8)

oPrint:Line (nRow2+0710,1800,nRow2+1800,1800 )
oPrint:Line (nRow2+1210,1800,nRow2+1210,2300 )
oPrint:Line (nRow2+1310,1800,nRow2+1310,2300 )
oPrint:Line (nRow2+1410,1800,nRow2+1410,2300 )
oPrint:Line (nRow2+1510,1800,nRow2+1510,2300 )
oPrint:Line (nRow2+1610,100 ,nRow2+1610,2300 )
oPrint:Line (nRow2+1800,100 ,nRow2+1800,2300 )



//MSBAR3("INT25",23.3,0.8,aCB_RN_NN[1],oPrint,.F.,Nil,Nil,0.027,1.1,Nil,Nil,"A",.F.)
//MSBAR("INT25",14.5,1.5,aCB_RN_NN[1],oPrint,.F.,   ,   ,0.027,1.1,   ,   ,   ,.F.)
//MSBAR("INT25",12.3,0.9,aCB_RN_NN[1],oPrint,.F.,,,0.0145,0.9,,,,.F.)
MSBAR("INT25",23.9,1.5,aCB_RN_NN[1],oPrint,.F.,,,0.0250,1.3,,,,.F.)

oPrint:EndPage() // Finaliza a p�gina

Return Nil

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  � Modulo10 � Autor � Microsiga             � Data � 13/10/03 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � IMPRESSAO DO BOLETO LASE DO ITAU COM CODIGO DE BARRAS      ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Microsiga                         ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/                                                                          


//------------------------------------------------------------------------------------------------------------------------------
//                                        CALCULO DO DIGITO VERIFICADO DA LINHA DIGITAVEL
//------------------------------------------------------------------------------------------------------------------------------
/*
ANEXO IV � C�LCULO DO D�GITO VERIFICADOR (DV) DA LINHA DIGIT�VEL
(M�DULO 10)
A representa��o num�rica do c�digo de barras � composta, por cinco campos, sendo os
tr�s primeiros amarrados por DVs e calculados pelo m�dulo 10, conforme segue:
a) O m�dulo 10 dever� ser utilizado para calcular o DV dos 03 (tr�s) primeiros campos
da linha digit�vel;
b) Os multiplicadores come�am com o n�mero 2 (dois), sempre pela direita, alternandose
1 e 2;
c) Multiplicar cada algarismo que comp�e o n�mero pelo seu respectivo peso
(multiplicador):
d) Caso o resultado da multiplica��o seja maior que 9 (nove) dever�o ser somados os
algarismos do produto, at� reduzi-lo a um �nico algarismo:
a. Exemplo: Resultado igual a 18, ent�o 1+8 = 9
e) Subtrair o total apurado no item anterior, da dezena imediatamente superior ao total
apurado:
a. Exemplo: Resultado da soma igual a 25, ent�o 30 - 25
f) O resultado obtido ser� o d�gito verificador do n�mero;
a. Exemplo: 30-25 = 5 ent�o 5 � o D�gito Verificador
g) Se o resultado da subtra��o for igual a 10 (dez), o d�gito verificador ser� igual a 0
(zero).
*/

Static Function Modulo10(cData)
LOCAL L,D,P,DZ := 0
LOCAL B     := .F.
L := Len(cData)
B := .T.
D := 0
While L > 0
	P := Val(SubStr(cData, L, 1))
	If (B)
		P := P * 2
		If P > 9  
		    P:= cValtoChar(P)
      		P:= Val(somastr(SubStr(P, 1, 1),SubStr(P, 2, 1)))   
      		
		End
	End     
	D := D + P
	L := L - 1
	B := !B
End               
    D:= cValtoChar(D)
    DZ:= (Val(SubStr(D, 1, 1)) + 1 )*10
    D:=DZ-Val(D)
If D = 10
	D := 0 
End
Return(D)

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  � Modulo11 � Autor � Joao Paulo Santos     � Data � 15/12/20 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � IMPRESSAO DO BOLETO BANCO DO BRASIL COM CODIGO DE BARRAS   ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Microsiga                         ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/                                   
//Regra bancaria:
/*/ 15/12/2020 - Joao Paulo Santos da Silva 
ANEXO V � C�LCULO DO D�GITO VERIFICADOR (DV) DO C�DIGO DE BARRAS (M�DULO 11)
Por defini��o do BACEN, na 5� posi��o do c�digo de barras, deve ser indicado, obrigatoriamente, o �d�gito verificador� (DV), do C�digo de Barras, calculado pelo m�dulo 11, conforme segue:
a) O c�digo de barras possui 44 (quarenta e quatro) posi��es, incluindo o DV;
b) Para calcular o DV considerar 43 posi��es do C�digo de Barras sendo da posi��o 1 a 4 e da posi��o 6 a 44;
c) Multiplicar cada algarismo que comp�e o n�mero pelo seu respectivo multiplicador (peso), iniciando-se pela 44a posi��o e saltando a 5a posi��o;
d) Os multiplicadores (pesos) variam de 2 a 9;
e) O primeiro d�gito da direita para a esquerda dever� ser multiplicado por 2, o segundo por 3 e assim sucessivamente;
f) Os resultados das multiplica��es devem ser somados:
a) Exemplo:(6 X 2) + (3 X 1) + (4 X 8) + ... + (4 X 0) = 712
g) O total da soma dever� ser dividido por 11:
a. Exemplo: 712/11 = 64. Resto igual a 8;
h) O resto da divis�o dever� ser subtra�do de 11:
a. Exemplo: 11 - 8 = 3, Portando �3� � o D�gito verificador
i) Se o resultado da subtra��o for:
I - igual a 0.....................D.V. igual a 1
II - igual a 10....................D.V. igual a 1
III - igual a 11....................D.V. igual a 1
IV - diferente de 10 e 11..........D.V. ser� o pr�prio d�gito, no caso do exemplo �3�
OBS: EM NENHUMA HIPOTESE PODER� SER UTILIZADO O D�GITO "0" (ZERO) NA QUINTA POSI��O DO C�DIGO DE BARRAS.
j) O resultado deste c�lculo dever� ser inclu�do na 5a posi��o do c�digo de barras.
/*/

Static Function Modulo11(cData,lNossoNum_DV)
Local L := Len(cdata)
Local D := 0
Local P := 1
While L > 0
	P := P + 1
	D := D + (Val(SubStr(cData, L, 1)) * P)
		If P = 9
			P := 1
		End
	L := L - 1
End
	D := mod(D,11)
    D := 11 - D
    
If D == 10 .OR. D == 0 .OR. D == 11 
	D := 1
EndIf
Return(D)

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  � mNossDv  � Autor � Joao Paulo Santos     � Data � 15/12/20 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � CALCULO DO DIGITO VERIFICADO DO NOSSO NUMERO               ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Wings                             ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/
//Regra:
//OB1. Jo�o paulo Santos da Silva, verifica nome da variav�l que est� recebendo os valores cData recebendo nosso numero,
//lNossoNum_DV recebendo valor booleano, analisar custo de troca.
//ANEXO VIII � COMPOSI��O DO CAMPO �NOSSO N�MERO� � CONV�NIO DE 7 POSI��ES
//FORMATO �NOSSO N�MERO� PARA CONV�NIOS ACIMA DE 1.000.000 (UM MILH�O): A composi��o do nosso n�mero deve obedecer as seguintes regras:
//CCCCCCCNNNNNNNNNN conv�nios com numera��o acima de 1.000.000, onde:
//"C" - � o n�mero do conv�nio fornecido pelo Banco (n�mero fixo e n�o pode ser alterado)
//"N" - � um sequencial atribu�do pelo cliente
//REGRA BANCARIA DO MODULO 11
//a) O m�dulo 11 � utilizado para montagem dos campos nosso-n�mero, c�digo do Benefici�rio e prefixo da ag�ncia, que ser�o impressos nos respectivos campos do boleto de cobran�a;
//b) Cada algarismo que comp�e o nosso-n�mero � multiplicado pelo respectivo multiplicador (peso):
//I. Os multiplicadores (pesos) variam de 9 a 2, da direita para a esquerda;
//II. O primeiro d�gito da direita para a esquerda dever� ser multiplicado por 9, o segundo por 8 e assim sucessivamente;
//c) Os resultados das multiplica��es s�o somados;
//d) O total da soma � divido por onze;
//I. Se o resto for menor que 10 (dez) o DV ser� igual ao resto;
//II. Se o resto for igual a 10 /dez/ o DV ser� igual a X;
//III. Se o resto for igual a 0 /zero/ o DV ser� igual a 0;
//------------------------------------------------------------------------------------------------------------------------------
//                                        CALCULO DO DIGITO VERIFICADO DO NOSSO NUMERO
//------------------------------------------------------------------------------------------------------------------------------

Static Function mNossDv(cData,lNossoNum_DV)
Local L := Len(cdata)
Local D := 0
Local P := 10
While L > 0 
	P := P - 1
	D := D + (Val(SubStr(cData, L, 1)) * P)
		If P = 2
			P := 10
		End
			L := L - 1
End           
	If lNossoNum_DV
		D := mod(D,11)
			If D == 0 
			// COMENTADO POR JOAO PAULO EM 14/02/2020 SEGUINRO A REGRA BANCARIA BB ACIMA
			//.OR. D == 1
	   			D := 0
			Endif 
			If (D>0 .or. D < 10)
	   			D := D
   			EndIf
	   		If D == 10
				D := 'x'  
			// ENDIF ADICIONADO POR JOAO PAULO
			Endif	
	EndIf 
Return(D)



/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Programa  �Ret_cBarra  � Autor � Microsiga           � Data � 15/12/20 ���
�������������������������������������������������������������������������Ĵ��
���Descri��o � IMPRESSAO DO BOLETO LASE DO BB COM CODIGO DE BARRAS        ���
�������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Cliente Wings                              ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

//[1]Numero do Banco[3]   Ag�ncia[4]    Conta CC[5]     D�gitocc      E1_NUMBCO    CALCULO   E1_VENCTO   EE_CODEMP
Static Function Ret_cBarra(cBanco,cAgencia,cConta,cDacCC,cNroDoc,nValor,dVencto,cConvenio)

LOCAL cValorFinal := strzero(nValor*100,10)
LOCAL nDvnn			:= 0
LOCAL nDvcb			:= 0
LOCAL nDv			:= 0
LOCAL cNN			:= ''
LOCAL cRN			:= ''
LOCAL cCB			:= ''
LOCAL cS			:= ''
LOCAL cFator        := strzero(dVencto - ctod("07/10/97"),4)
LOCAL cCart		 	:= "17"
LOCAL cMoeda        := "9" 
Local cBanco        := "001"        

//------------------------------------------------------------------------------------------------------------------------------
//                                                   DEFINI��O DO NOSSO NUMERO
//------------------------------------------------------------------------------------------------------------------------------
//14/12/2020
//OB1. Jo�o paulo Santos da Silva, verifica nome da variav�l que est� recebendo os valores cData recebendo nosso numero,
//lNossoNum_DV recebendo valor booleano, analisar custo de troca.
//ANEXO VIII � COMPOSI��O DO CAMPO �NOSSO N�MERO� � CONV�NIO DE 7 POSI��ES
//FORMATO �NOSSO N�MERO� PARA CONV�NIOS ACIMA DE 1.000.000 (UM MILH�O): A composi��o do nosso n�mero deve obedecer as seguintes regras:
//CCCCCCCNNNNNNNNNN conv�nios com numera��o acima de 1.000.000, onde:
//"C" - � o n�mero do conv�nio fornecido pelo Banco (n�mero fixo e n�o pode ser alterado)
//"N" - � um sequencial atribu�do pelo cliente
//REGRA BANCARIA DO MODULO 11
//a) O m�dulo 11 � utilizado para montagem dos campos nosso-n�mero, c�digo do Benefici�rio e prefixo da ag�ncia, que ser�o impressos nos respectivos campos do boleto de cobran�a;
//b) Cada algarismo que comp�e o nosso-n�mero � multiplicado pelo respectivo multiplicador (peso):
//I. Os multiplicadores (pesos) variam de 9 a 2, da direita para a esquerda;
//II. O primeiro d�gito da direita para a esquerda dever� ser multiplicado por 9, o segundo por 8 e assim sucessivamente;
//c) Os resultados das multiplica��es s�o somados;
//d) O total da soma � divido por onze;
//I. Se o resto for menor que 10 (dez) o DV ser� igual ao resto;
//II. Se o resto for igual a 10 /dez/ o DV ser� igual a X;
//III. Se o resto for igual a 0 /zero/ o DV ser� igual a 0;

cS    := SubStr(cConvenio,1,7) + AllTrim(StrZero(Val(cNroDoc),10))
nDvnn := mNossDv(cS,.T.)      // Corrigido por joao paulo em 14/12/2020 para atender as regras bancaria do BB
cNN   := cS                   // Alterado por Jo�o Paulo em 14/12/2020 n�o necess�rio uso de DV + AllTrim(Str(nDvnn))

//------------------------------------------------------------------------------------------------------------------------------
//                                                 DEFINI��O DO C�DIGO DE BARRAS
//------------------------------------------------------------------------------------------------------------------------------
/* 14/12/2020
Posi��o  Tamanho  Picture      Conte�do
01 a 03    03      9(03)       C�digo do Banco na C�mara de Compensa��o = '001' 
04 a 04    01      9(01)       C�digo da Moeda = 9 (Real)
05 a 05    01      9(01)       Digito Verificador (DV) do c�digo de Barras* 
06 a 09    04      9(04)       Fator de Vencimento **
10 a 19    10      9(08)V(2)   Valor 
20 a 44    03      9(03)       Campo Livre *** 
*/

//cS    := cBanco + cMoeda + cFator +  cValorFinal + "000000" + substr(cConvenio,1,7) + strzero(val(right(cNN,8)),10) + cCart
cS    := cBanco + cMoeda + cFator +  cValorFinal + "000000" + cNN + cCart
	nDvcb := modulo11(cS,.F.)
		cCB   := SubStr(cS, 1, 4) + AllTrim(Str(nDvcb)) + SubStr(cS,5,43)


//------------------------------------------------------------------------------------------------------------------------------
//	                                            DEFIN��O DA LINHA DIGITAVEL
//------------------------------------------------------------------------------------------------------------------------------
// REPRESENTA��O NUMERICA    
// AAABC.CCCCX DDDDD.DDDDDY EEEEE.EEEEEZ K UUUUVVVVVVVVVV
// Campo 1       Campo 2      Campo 3    4     Campo 5

// CAMPO 1: AAABC.CCCCX
// A = N�mero C�digo da IF Destinat�ria no SILOC
// B = C�digo da moeda (9) -Real
// C = Posi��es 20 a 24 do c�digo de barras
// X = DV do Campo 1 (calculado de acordo com o M�dulo 10 � anexo IV)

cS     := cBanco + cMoeda + substr(cCB,20,5)
	nDv    := modulo10(cS)
		cRN    := left(cS,5) + "." + right(cS,4) + AllTrim(Str(nDv)) + " "

// CAMPO 2:
// Campo 2: DDDDD.DDDDDY
// D = Posi��es 25 a 34 do c�digo de barras
// Y = DV do Campo 2 (calculado de acordo com o M�dulo 10 � anexo IV)

cS    := substr(cCB,25,10)
	nDv   := modulo10(cS)
		cRN   := cRN + left(cS,5) + "." + right(cS,5) + AllTrim(Str(nDv)) + " "

// CAMPO 3: EEEEE.EEEEEZ
// F = Posi��es 35 a 44 do c�digo de barras
// Z =DV do Campo 3 (calculado de acordo com o M�dulo 10 � anexo IV)

cS    := substr(cCB,35,10)
	nDv   := modulo10(cS)
		cRN   := cRN + left(cS,5) + "." + right(cS,5) + AllTrim(Str(nDv)) + " "

// CAMPO 4: K
// K = DV do c�digo de barras (calculado de acordo com o M�dulo 11 � anexo VI)

cRN   := cRN + AllTrim(Str(nDvcb)) + " "

// CAMPO 5: UUUUVVVVVVVVVV
// U = Fator de Vencimento (c�lculo conforme anexo V)
// V = Valor do boleto de pagamento (com duas casas decimais, sem ponto e v�rgula. Em caso de moeda vari�vel, informar zeros)

cRN   := cRN + cFator + StrZero((nValor*100),10)


Return({cCB,cRN,cNN})

/*/
���������������������������������������������������������������������������������
�����������������������������������������������������������������������������Ŀ��
���Fun��o    � AjustaSx1    � Autor � Microsiga            	� Data � 13/10/03 ���
�����������������������������������������������������������������������������Ĵ��
���Descri��o � Verifica/cria SX1 a partir de matriz para verificacao          ���
�����������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Microsiga                    	  	  ���
������������������������������������������������������������������������������ٱ�
���������������������������������������������������������������������������������
����������������������������������������������������������������������������������
/*/
Static Function AjustaSX1(cPerg, aPergs)

Local _sAlias	:= Alias()
Local aCposSX1	:= {}
Local nX 		:= 0
Local lAltera	:= .F.
Local nCondicao
Local cKey		:= ""
Local nJ			:= 0

aCposSX1:={"X1_PERGUNT","X1_PERSPA","X1_PERENG","X1_VARIAVL","X1_TIPO","X1_TAMANHO",;
"X1_DECIMAL","X1_PRESEL","X1_GSC","X1_VALID",;
"X1_VAR01","X1_DEF01","X1_DEFSPA1","X1_DEFENG1","X1_CNT01",;
"X1_VAR02","X1_DEF02","X1_DEFSPA2","X1_DEFENG2","X1_CNT02",;
"X1_VAR03","X1_DEF03","X1_DEFSPA3","X1_DEFENG3","X1_CNT03",;
"X1_VAR04","X1_DEF04","X1_DEFSPA4","X1_DEFENG4","X1_CNT04",;
"X1_VAR05","X1_DEF05","X1_DEFSPA5","X1_DEFENG5","X1_CNT05",;
"X1_F3", "X1_GRPSXG", "X1_PYME","X1_HELP" }

dbSelectArea("SX1")
dbSetOrder(1)
For nX:=1 to Len(aPergs)
	lAltera := .F.
	If MsSeek(cPerg+Right(aPergs[nX][11], 2))
		If (ValType(aPergs[nX][Len(aPergs[nx])]) = "B" .And.;
			Eval(aPergs[nX][Len(aPergs[nx])], aPergs[nX] ))
			aPergs[nX] := ASize(aPergs[nX], Len(aPergs[nX]) - 1)
			lAltera := .T.
		Endif
	Endif
	
	If ! lAltera .And. Found() .And. X1_TIPO <> aPergs[nX][5]
		lAltera := .T.		// Garanto que o tipo da pergunta esteja correto
	Endif
	
	If ! Found() .Or. lAltera
		RecLock("SX1",If(lAltera, .F., .T.))
		Replace X1_GRUPO with cPerg
		Replace X1_ORDEM with Right(aPergs[nX][11], 2)
		For nj:=1 to Len(aCposSX1)
			If 	Len(aPergs[nX]) >= nJ .And. aPergs[nX][nJ] <> Nil .And.;
				FieldPos(AllTrim(aCposSX1[nJ])) > 0
				Replace &(AllTrim(aCposSX1[nJ])) With aPergs[nx][nj]
			Endif
		Next nj
		MsUnlock()
		cKey := "P."+AllTrim(X1_GRUPO)+AllTrim(X1_ORDEM)+"."
		
		If ValType(aPergs[nx][Len(aPergs[nx])]) = "A"
			aHelpSpa := aPergs[nx][Len(aPergs[nx])]
		Else
			aHelpSpa := {}
		Endif
		
		If ValType(aPergs[nx][Len(aPergs[nx])-1]) = "A"
			aHelpEng := aPergs[nx][Len(aPergs[nx])-1]
		Else
			aHelpEng := {}
		Endif
		
		If ValType(aPergs[nx][Len(aPergs[nx])-2]) = "A"
			aHelpPor := aPergs[nx][Len(aPergs[nx])-2]
		Else
			aHelpPor := {}
		Endif
		
		PutSX1Help(cKey,aHelpPor,aHelpEng,aHelpSpa)
	Endif
Next

/*/
���������������������������������������������������������������������������������
�����������������������������������������������������������������������������Ŀ��
���Fun��o    � CONTASAN     � Autor � Alexandre            	� Data � 10/04/08 ���
�����������������������������������������������������������������������������Ĵ��
���Descri��o � AJUSTA O NUMERO DA CONTA TIRA OS "-" QUE O USUARIO COLOCO      ���
�����������������������������������������������������������������������������Ĵ��
���Uso       � Especifico para Clientes Microsiga                    	  	  ���
������������������������������������������������������������������������������ٱ�
���������������������������������������������������������������������������������
����������������������������������������������������������������������������������
/*/
Static FUNCTION CONTASAN(cCONTA)
cCONTA := PADL(LEFT(cCONTA,2)+"0"+STRTRAN(SUBSTR(cCONTA,4,LEN(cCONTA)),"-",""),10,"0")
RETURN(cCONTA)


Static Function ValidPerg()
_sAlias := Alias()
DBSelectArea("SX1")
DBSetOrder(1)
cPerg := "BOLSBB"
cPerg := PADR(cPerg,10)
aRegs:={}

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
Aadd(aRegs,{cPerg,"01","Do Prefixo           ?","","","mv_ch1","C", 3,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","",""})
Aadd(aRegs,{cPerg,"02","Ate o Prefixo        ?","","","mv_ch2","C", 3,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","",""})
Aadd(aRegs,{cPerg,"03","Do Titulo            ?","","","mv_ch3","C", 9,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","",""})
AADD(aRegs,{cPerg,"04","Ate o Titulo         ?","","","mv_ch4","C", 9,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","",""})
AADD(aRegs,{cPerg,"05","Da Parcela           ?","","","mv_ch5","C", 1,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","",""})
AADD(aRegs,{cPerg,"06","Ate a Parcela        ?","","","mv_ch6","C", 1,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","",""})
AADD(aRegs,{cPerg,"07","Do Bordero           ?","","","mv_ch7","C", 6,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","",""})
AADD(aRegs,{cPerg,"08","Ate o Bordero        ?","","","mv_ch8","C", 6,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","",""})


For i:=1 to Len(aRegs)
	If ! DBSeek(cPerg+aRegs[i,2])
		RecLock("SX1",.T.)
		For j:=1 to Max(39, Len(aRegs[i])) //fCount()
			FieldPut(j,aRegs[i,j])
		Next
		MsUnlock()
	Endif
Next

DBSkip()
             //verificar uso de vari�vel x1_grupo a mesma se encontra sem referencia att, joao paulo santos
do while x1_grupo == cPerg
	RecLock("SX1", .F.)
	DBDelete()
	DBSkip()
Enddo

DBSelectArea(_sAlias)

Return


static function SMPESSOA(p_cCLIENTE,p_cLOJA)
// Criada por Fernando Lins em: 28-06-2017
// Objetivo, trazer apenas clientes que possuam cadastros do tipo pessoa juridica A1_PESSOA = "J"
// chamado criado pela wings para a Semar, N�:

Local cPessoa := "F"

/*local lRet:= .f.

wAliasQRY:= "XSA1"

cSq02 := " SELECT * "
cSq02 += " FROM " + RetSqlName("SA1")
cSq02 += " WHERE D_E_L_E_T_ = ' ' "
cSq02 += "   AND A1_FILIAL = '" + xFilial('SA1')+ "'"
cSq02 += "   AND A1_COD = '" + p_cCLIENTE  + "'"
cSq02 += "   AND A1_LOJA = '" + p_cLOJA + "' "
cSq02 += "   AND A1_PESSOA = 'J' "

If Select(wAliasQRY)>0
DbSelectArea(wAliasQRY)
DbCloseArea()
Endif
TcQuery cSq02 New Alias (wAliasQRY)

lRet:= !empty((wAliasQRY)->A1_COD) */
cPessoa := POSICIONE("SA1", 1, xFilial("SA1")+p_cCLIENTE+p_cLOJA,"A1_PESSOA")


return cPessoa

