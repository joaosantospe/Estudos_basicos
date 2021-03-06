#include "totvs.ch"
#include "protheus.ch"
#include "TOPCONN.CH"

User Function fImpCsv() 
Local cDiret
Local cLinha  := ""
Local lPrimlin   := .T.
Local aCampos := {}
Local aDados  := {}
Local i
Local j 
Private aErro := {}
cDiret :=  cGetFile( 'Arquito CSV|*.csv| Arquivo TXT|*.txt| Arquivo XML|*.xml',; //[ cMascara], 
                         'Selecao de Arquivos',;                                 //[ cTitulo], 
                         0,;                                                     //[ nMascpadrao], 
                         'C:',;                                                  //[ cDirinicial], 
                         .F.,;                                                   //[ lSalvar], 
                         GETF_LOCALHARD  + GETF_NETWORKDRIVE,;                   //[ nOpcoes], 
                         .T.)                                                    //[arvore]

//INDICA O NOME DO ARQUIVO TXT QUE SER� ABERTO                         
	FT_FUSE(cDiret)ADMIN

//DEFINE TAMANHO DA REGUA(L� O NUMERO TOTAL DE LINHAS DO ARQUIVO TEXTO ABERTO)
	ProcRegua(FT_FLASTREC())

//POSICIONA NO PRIMEIRO CARACTER DA PRIMEIRA LINHA 
	FT_FGOTOP()

// ENQUANTO PONTEIRO N�O ESTIVER NO FIM DO ARQUIVO TXT
	While !FT_FQUIVOEOF()
 
	IncProc("Lendo arquivo texto...")
    // CLINHA L� E RETORNA UMA LINHA DO ARQUIVO TXT ABERTO
	cLinha := FT_FREADLN()
 
	If lPrimlin
         // ACAMPOS RECEBE LINHA SEPARADA POR POSTO E VIRGULA .T. CONSIDERANDO VAZIO
		aCampos := Separa(cLinha,";",.T.)
		lPrimlin := .F.
	Else
		AADD(aDados,Separa(cLinha,";",.T.))
	EndIf
 
     //MOVE PONTEIRO PARA NOVA LINHA
	FT_FSKIP()
EndDo
 
Begin Transaction
	ProcRegua(Len(aDados))
	For i:=1 to Len(aDados)
 
		IncProc("Importando Registros...")
 
		dbSelectArea("SA1")
		dbSetOrder(1)
		dbGoTop()

		If !dbSeek(xFilial("SA1")+aDados[i,1]+aDados[i,2])
			Reclock("SA1",.T.)
			SA1->A1_FILIAL := xFilial("SA1")
			For j:=1 to Len(aCampos)
				cCampo  := "SA1->" + aCampos[j] //SA1->A1_COD
				&cCampo := aDados[i,j] //SA1->A1_COD := 000008   SA1->A1_LOJA := 01 SA1->A1_NOME := JOSE
			Next j
			SA1->(MsUnlock())
		EndIf
	Next i
End Transaction
  
ApMsgInfo("Importa��o conclu�da com sucesso!","Sucesso!")
 
Return
