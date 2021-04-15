#include "topconn.ch"
#include "protheus.ch"
#include "totvs.ch"

user function importsb1()
local cDiret 
local aDados :={}
local aCampos :={}
local i
local j

cDiret := cGetFile('Arquivos de cvs|*.cvs| Arquivo txt|*.txt| Arquivo xml|*.xml',;  //[cMascara],
                                                           'Selecione o arquivo',;  //[Titulo do Box],  
                                                                               0,;  //[Mascara padrão],
                                                                     'c:\totvs\',;  //[Diretorio], 
                                                                             .F.,;  //[Salvar],
                                             GETF_LOCALHARD  + GETF_NETWORKDRIVE,;  //[Opcoes],
                                                                             .T.)   //[Arvore]

MsgInfo("Você selecionou o arquivo"+cDiret, "Atençao")

Return





