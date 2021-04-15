include 'topconn.ch'
include 'protheus.ch'


user function importsb1()
local cDiret 
local aDados ={}
local aCampos = {}
local i
local j

cDiret= cGetFile("Arquivo t | *.txt | Arquivos de C | *.cvs | Arquivo x | *.xml",;  //Mascara
                                                             "Selecioneoarquivo",;  //Titulo do Box  
                                                                               0,;  //Mascara padrão
                                                                       c:\totvs\,;  //Diretorio 
                                                                             .F.,;  //Salvar
                                             GETF_LOCALHARD  + GETF_NETWORKDRIVE,;  //Opcoes
                                                                             .T.)   //Arvore

MsgInfo("Você selecionou o arquivo" +cDirect,"Atençao")

Return()





