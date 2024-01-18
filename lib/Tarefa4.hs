{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Diogo Mota Moreira <a106841@alunos.uminho.pt>
              Nuno Gil de Magalhães Baldaia Mendes <a106875@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- primeira lista são ações de inimigos, segunda é a acçao a aplicar a jogador 
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acaoI acaoJ jogo = jogo{jogador = verifyJ acaoJ jogo , inimigos = verifyI acaoI (inimigos jogo)}


verifyJ:: Maybe Acao -> Jogo -> Personagem 
verifyJ (Just Saltar)        jogo = if buscaElementoCartesiano (blocosM (mapa jogo)) (x,y) == Just Vazio && (buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Plataforma || buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Escada || buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Alcapao) 
                                    then pers{velocidade = (xv,yv+0.2)}
                                    else pers
                                    where (x,y) = posicao pers
                                          (xv,yv) = velocidade pers                                         
                                          pers = jogador jogo   

-- ter atenção a este nothing --> Se este nothing não funcionar temos de mudar a buscar elemento cartesiano para quando este entra em contacto com a hitbox
verifyJ (Just AndarDireita)  jogo = if buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Plataforma || buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Alcapao || buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Escada && buscaElementoCartesiano (blocosM (mapa jogo)) (x,y) == Just Vazio
                                    then (pers){direcao = Este , velocidade = (0.1,0)}
                                    else if buscaElementoCartesiano (blocosM (mapa jogo)) (posicao pers) == Just Escada && buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Plataforma
                                         then (pers){direcao = Este , velocidade = (0.1,0)}
                                         else pers
                                    where (x,y) = posicao pers
                                          (xv,yv) = velocidade pers 
                                          pers = jogador jogo

verifyJ (Just AndarEsquerda) jogo = if buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Plataforma || buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Alcapao || buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Escada && buscaElementoCartesiano (blocosM (mapa jogo)) (x,y) == Just Vazio
                                    then (pers){direcao = Oeste , velocidade = (-0.1,0)}
                                    else if buscaElementoCartesiano (blocosM (mapa jogo)) (posicao pers) == Just Escada && buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Plataforma
                                         then (pers){direcao = Oeste , velocidade = (-0.1,0)}
                                         else pers
                                    where (x,y) = posicao pers
                                          (xv,yv) = velocidade pers
                                          pers = jogador jogo 

verifyJ (Just Subir)         jogo =  if buscaElementoCartesiano (blocosM (mapa jogo)) (x,y)   == Just Escada || buscaElementoCartesiano (blocosM (mapa jogo)) (x, y-1) == Just Escada 
                                         then pers{direcao = Norte , velocidade = (0,0.1)} 
                                         else pers
                                         where (x,y) = posicao pers
                                               pers = jogador jogo

verifyJ (Just Descer)        jogo = if buscaElementoCartesiano (blocosM (mapa jogo)) (x,y) == Just Escada && buscaElementoCartesiano (blocosM (mapa jogo)) (x,y-1) == Just Plataforma 
                                    then (pers){velocidade = (0,0)}
                                    else if buscaElementoCartesiano (blocosM (mapa jogo)) (x,y) == Just Escada || ((buscaElementoCartesiano (blocosM (mapa jogo)) (x, y-1) == Just Escada) && (buscaElementoCartesiano (blocosM (mapa jogo)) (x,y)   == Just Vazio))
                                         then (pers){direcao = Sul , velocidade = (0,-0.1)}
                                         else pers
                                    where (x,y) = posicao pers
                                          pers = jogador jogo
                                         
verifyJ (Just Parar)         jogo = pers{velocidade = (0,0)}
                                    where (x,y) = posicao pers
                                          pers = jogador jogo

verifyJ (Nothing)            jogo = (jogador jogo)

verifyI:: [Maybe Acao] -> [Personagem] -> [Personagem] 
verifyI [] _ = []
verifyI (h:t) (hi:ti) = (verifyIAux h hi): (verifyI t ti)

verifyIAux:: Maybe Acao -> Personagem -> Personagem
verifyIAux a inimigo = case a of
      (Just Subir)         -> inimigo {velocidade = (vx,0.05)}
      (Just Descer)        -> inimigo {velocidade = (vx,-0.05)}
      (Just AndarDireita)  -> inimigo {velocidade = (0.05,vy)}
      (Just AndarEsquerda) -> inimigo {velocidade = (-0.05,vy)}
      (Just Parar)         -> inimigo {velocidade = (0,0)}
      _                    -> inimigo
      where (vx,vy) = velocidade inimigo

-- 1. 
eventJ :: Event -> PrimateKong -> Maybe Acao
eventJ (EventKey(SpecialKey KeySpace) Down _ _) pk = if (tempo pk) < 0.02 then Just Saltar else Nothing
eventJ (EventKey(SpecialKey KeySpace) Up _ _)   pk = Nothing
eventJ (EventKey(SpecialKey KeyRight) Down _ _) pk = Just AndarDireita
eventJ (EventKey(SpecialKey KeyRight) Up _ _)   pk = Just Parar
eventJ (EventKey(SpecialKey KeyLeft ) Down _ _) pk = Just AndarEsquerda
eventJ (EventKey(SpecialKey KeyLeft ) Up _ _)   pk = Just Parar 
eventJ (EventKey(SpecialKey KeyUp   ) Down _ _) pk = Just Subir
eventJ (EventKey(SpecialKey KeyUp   ) Up _ _)   pk = Just Parar
eventJ (EventKey(SpecialKey KeyDown ) Down _ _) pk = Just Descer
eventJ (EventKey(SpecialKey KeyDown ) Up _ _)   pk = Just Parar 
eventJ _                                        pk = Nothing 
      
-- 2. 
eventI :: [Personagem]-> [Maybe Acao]
eventI [] = []
eventI (h:t) = case direcao h of 
    Norte -> [Just Subir] ++ eventI t
    Sul   -> [Just Descer] ++ eventI t
    Este  -> [Just AndarDireita] ++ eventI t
    Oeste -> [Just AndarEsquerda] ++ eventI t