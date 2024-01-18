{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Diogo Mota Moreira <a106841@alunos.uminho.pt>
              Nuno Gil de Magalhães Baldaia Mendes <a106875@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1

{-
A funçãoao valida deve verificar que um dado jogo nao viola nenhuma das seguintes restrições:
1. O mapa tem “chão”, i.e. uma plataforma que impede que o jogador ou outro personagem caia fora do mapa.

2. Todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.

3. A posição inicial de um jogador não pode colidir com a posição inicial de um outro personagem. Note que as posiçoes iniciais de inimigos
podem colidir entre estes.

4. Numero minimo de inimigos: 2 (dois.)

5. Inimigos Fantasma têm exactamente 1 (uma) vida.

6. Escadas nao podem começar/terminar em alcapões, e pelo menos uma das suas extremidades tem que ser do tipo Plataforma.

7. Alçapoes nao podem ser menos largos que o jogador.

8. Nao podem existir personagens nem coleccionaveis “dentro” de plataformas ou alcapoes, i.e. o bloco (na matriz do mapa) correspendente
a posiçao de um personagem ou objecto tem que ser Vazio.
-}


valida :: Jogo -> Bool
valida jogo = 
    verificaChao              (mapa jogo) &&
    verificaRessaltaInimigos  (inimigos jogo) && 
    verificaRessaltaJogador   (jogador jogo) &&
    verificaPosInicial        (mapa jogo) (jogador jogo) (inimigos jogo) &&
    verificaNumMinimoInimigos (inimigos jogo) &&
    verificaVidaInimigos      (inimigos jogo) &&
    verificaEscadas           (mapa jogo) 0 &&
    verificaTamanhoPers       (jogador jogo) &&
    verificaPersVazio         (mapa jogo) ((inimigos jogo) ++ [jogador jogo]) &&
    verificaColecVazio        (mapa jogo) (colecionaveis jogo)


    
    


--    1. 
verificaChao:: Mapa -> Bool 
verificaChao (Mapa (_ , _) _ blocos) = verificaChaoAux (blocos !! 9)

verificaChaoAux :: [Bloco] -> Bool
verificaChaoAux [] = False
verificaChaoAux [h] = h == Plataforma
verificaChaoAux (h:t) 
    |h == Plataforma = verificaChaoAux t
    |otherwise       = False


--    2.
verificaRessaltaInimigos:: [Personagem] -> Bool
verificaRessaltaInimigos [] = True
verificaRessaltaInimigos (h:t) = ressalta h == True && verificaRessaltaInimigos t

verificaRessaltaJogador:: Personagem -> Bool
verificaRessaltaJogador jog = ressalta jog == False

--    3. 
verificaPosInicial:: Mapa -> Personagem -> [Personagem] -> Bool
verificaPosInicial _ _ [] = True
verificaPosInicial (Mapa ((xi,yi), dir) posf mapa) jogador (h:t) 
    | hitboxColisao (createHitbox (xi,yi) (l1,c1)) (createHitbox (posicao h) (tamanho h)) == False = verificaPosInicial (Mapa ((xi,yi), dir ) posf mapa) jogador t
    | otherwise = False
    where (l1,c1)           = tamanho jogador
--    4.
verificaNumMinimoInimigos :: [Personagem] -> Bool
verificaNumMinimoInimigos inimigos = length inimigos >= 2

--    5.
verificaVidaInimigos :: [Personagem] -> Bool 
verificaVidaInimigos [] = True
verificaVidaInimigos inimigos 
    | vida h == 1 = verificaVidaInimigos t
    | otherwise   = False
    where (h:t) = inimigos

--    6. 
-- Int que lê é para começar a buscar as colunas desde o 0 e fazer contagem por aí adiante 

verificaEscadas :: Mapa -> Int -> Bool
verificaEscadas (Mapa ((x,y), b) e []) _ = True
verificaEscadas (Mapa ((x,y), b) e ([]:s:t)) c = verificaEscadas (Mapa ((x,y), b) e (s:t)) 0
verificaEscadas (Mapa ((x,y), b) e (h:[]:t)) c = verificaEscadas (Mapa ((x,y), b) e (h:t)) 0
verificaEscadas (Mapa ((x,y), b) e (h:s:t)) c 
    |(length h) <= c || (length s) <= c = verificaEscadas (Mapa ((x,y), b) e (s:t)) 0
    |h!!c == Alcapao && s!!c == Escada = False 
    |h!!c == Plataforma || h!!c == Vazio && s!!c == Escada && verificaEscadasAux t c = verificaEscadas (Mapa ((x,y), b) e (h:s:t)) (c+1)
    |otherwise = verificaEscadas (Mapa ((x,y), b) e (h:s:t)) (c+1)
verificaEscadas _ _ = True

verificaEscadasAux:: [[Bloco]] -> Int -> Bool
verificaEscadasAux [] _ = False
verificaEscadasAux ([]:t) c = verificaEscadasAux t c
verificaEscadasAux (h:t) c 
    | (h!!c) == Plataforma = True
    | (h!!c) == Escada = verificaEscadasAux t c
    | otherwise = False 



--    7.
{- para um personagem ser menos largo que um alcapão, vamos dizer que a largura de um 
   Personagem não pode exceder as 10 unidades, já que um alcapão irá, por definição -}
verificaTamanhoPers:: Personagem -> Bool
verificaTamanhoPers pers = largura <= 1
    where (largura,comprimento) = tamanho pers

--    8.
verificaPersVazio :: Mapa -> [Personagem]->  Bool
verificaPersVazio _ [] = True
verificaPersVazio (Mapa (_ , _) _ []) _ = False
verificaPersVazio (Mapa ((x,y), b) e blocos) (h:t)
    |buscaElementoCartesiano blocos (posicao h) == Just Vazio = verificaPersVazio (Mapa ((x,y), b) e blocos) t
    |otherwise = False

verificaColecVazio :: Mapa-> [(Colecionavel,Posicao)] -> Bool
verificaColecVazio _ [] = True
verificaColecVazio (Mapa (_ , _) _ []) _ = False
verificaColecVazio (Mapa ((x,y), b) e blocos) ((colec,pos):t)
    |buscaElementoCartesiano blocos pos == Just Vazio = verificaColecVazio (Mapa ((x,y), b) e blocos) t
    |otherwise = False
