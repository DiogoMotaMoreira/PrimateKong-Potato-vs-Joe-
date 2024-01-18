{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Diogo Mota Moreira <a106841@alunos.uminho.pt>
              Nuno Gil de Magalhães Baldaia Mendes <a106875@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import Tarefa1
import LI12324

blocosM :: Mapa -> [[Bloco]]
blocosM (Mapa _ _ b) = b

movimenta :: Semente -> Tempo -> Jogo -> Jogo 
movimenta sem temp jogo = 
    jogo {
         jogador       = (colisaoJ (mapa jogo) (efeitoGravidadeJ (mapa jogo) temp (colisaoJI (mapa jogo) (colecionavelPers (colecionaveis jogo) (jogador jogo)) (inimigos jogo))))
        ,mapa          = alcapao (mapa jogo) ([movimentaAuxP (mapa jogo) (jogador jogo)]++(movimentaAuxE sem (blocosM (mapa jogo)) (inimigos jogo)))
        ,inimigos      = (colisaoI sem (mapa jogo) (removeInimigo (ataqueJogador (jogador jogo) (efeitoGravidadeI (mapa jogo) temp (inimigos jogo)))))
        ,colecionaveis = colecionavelF (colecionaveis jogo) (movimentaAuxP (mapa jogo) (jogador jogo))
        }


movimentaAuxE:: Semente -> [[Bloco]] -> [Personagem] -> [Personagem]
movimentaAuxE _ _ [] = []
movimentaAuxE sem blocos (h:t) = h{posicao= (x+vx,y+vy)} : movimentaAuxE sem blocos t
    where (x,y) = posicao h
          (vx,vy) = velocidade h

movimentaAuxP:: Mapa -> Personagem -> Personagem
movimentaAuxP m h = case direcao h of 
    Norte -> if buscaElementoCartesiano blocos (posicao h) == Just Escada || (buscaElementoCartesiano blocos (posicao h) == Just Vazio && buscaElementoCartesiano blocos (x,y-1) == Just Escada && y <= fromInteger(floor y) +0.5)
             then h{posicao = (x,y+vy)}
             else h
    Sul   -> if buscaElementoCartesiano blocos (posicao h) == Just Escada && buscaElementoCartesiano blocos (x,y-1) == Just Plataforma && y <= fromInteger(floor y) +0.5
             then h
             else h{posicao = (x +vx,y+vy)}
    _     -> h{posicao = (x+vx,y+vy)}
    where (x,y)    = posicao h
          (vx,vy)  = velocidade h
          Mapa pi pf blocos = m

-- 1.
ataqueJogador:: Personagem -> [Personagem] -> [Personagem]
ataqueJogador _ [] = [] 
ataqueJogador jog (h:t) 
    |ataqueJogadorAux jog h = [h {vida = vi-1}] ++ ataqueJogador jog t
    |otherwise = [h] ++ ataqueJogador jog t
    where vi = vida h 

-- True existe ataque 
-- False impossivel de atacar
ataqueJogadorAux:: Personagem -> Personagem -> Bool 
ataqueJogadorAux jogador inimigo = if possibilidadeAtaque jogador then (colisoesPersonagens jogador{posicao = p} inimigo ) else False
    where p = case direcao jogador of
              Norte -> (x,y+comprimento)
              Sul   -> (x,y-comprimento)
              Este  -> (x+largura,y)
              Oeste -> (x-largura,y)
          (x,y) = posicao jogador
          (largura,comprimento) = tamanho jogador
          

possibilidadeAtaque :: Personagem -> Bool
possibilidadeAtaque jog = a == True && b > 0
    where (a,b) = aplicaDano jog



-- 2. 
removeInimigo:: [Personagem] -> [Personagem]
removeInimigo [] = []
removeInimigo (h:t) 
    |vida h > 0 = [h] ++ removeInimigo t
    |otherwise = [desapareceMapa h] ++ removeInimigo t


desapareceMapa :: Personagem -> Personagem
desapareceMapa pers = pers {posicao = (-1000,-1000), velocidade = (0,0)}


-- 3. 
efeitoGravidadeJ::Mapa -> Tempo -> Personagem -> Personagem
efeitoGravidadeJ mapa temp pers
    |buscaElementoCartesiano (blocosM mapa) (x,y) == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Escada  = pers{velocidade = (vx,vy)}
    |buscaElementoCartesiano (blocosM mapa) (x,y) == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Vazio = pers{velocidade = (vx, vy - (snd gravidade)*temp)}
    |buscaElementoCartesiano (blocosM mapa) (x,y) == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Plataforma = if yc > (fromInteger(floor y) +0.5) then pers{velocidade = (vx, vy - (snd gravidade)*temp)} else pers{posicao = (x,fromInteger(floor y) +0.5),velocidade = (vx,0)}
    |buscaElementoCartesiano (blocosM mapa) (x,y) == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Alcapao = if yc > (fromInteger(floor y) +0.5) then pers{velocidade = (vx, vy - (snd gravidade)*temp)} else pers{posicao = (x,fromInteger(floor y) +0.5),velocidade = (vx,0)}
    |buscaElementoCartesiano (blocosM mapa) (x,y) == Nothing = pers {posicao = (x,-8.5),velocidade = (vx,vy)}
    |otherwise = pers
    where (x,y) = posicao pers
          (vx,vy) = velocidade pers
          yc = y + vy



efeitoGravidadeI::Mapa -> Tempo -> [Personagem] -> [Personagem]
efeitoGravidadeI mapa _ [] = []
efeitoGravidadeI mapa temp (h:t) 
    |buscaElementoCartesiano (blocosM mapa) (x,y)  == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Escada && direcao h /= Sul = [h{posicao = (x,fromInteger(floor y) +0.5),velocidade = (vx,0)}] ++ efeitoGravidadeI mapa temp t
    |buscaElementoCartesiano (blocosM mapa) (x,y)  == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Vazio = [h{velocidade = (vx, vy - (snd gravidade)*temp)}] ++ efeitoGravidadeI mapa temp t
    |(buscaElementoCartesiano (blocosM mapa) (x,y) == Just Vazio && buscaElementoCartesiano (blocosM mapa) (x,y-1) == Just Plataforma) = if yc > (fromInteger(floor y) +0.5) then [h{velocidade = (vx, vy - (snd gravidade)*temp)}] ++ efeitoGravidadeI mapa temp t else [h{posicao = (x,fromInteger(floor y) +0.5),velocidade = (vx,0)}] ++ efeitoGravidadeI mapa temp t
    |buscaElementoCartesiano (blocosM mapa) (x,y)  == Nothing = h {posicao = (x,-8.5),velocidade = (vx,0)} : efeitoGravidadeI mapa temp t
    |otherwise = h : efeitoGravidadeI mapa temp t
    where (x,y) = posicao h
          (vx,vy) = velocidade h
          yc = y + vy


-- 4. 
colisaoJI:: Mapa -> Personagem -> [Personagem] -> Personagem
colisaoJI _ jg [] = jg
colisaoJI m jog (h:t) 
    |colisoesPersonagens jog h = colisaoJI m (jog {vida = vi -1, posicao = pi, direcao = dir}) t 
    |otherwise = colisaoJI m jog t
    where vi = vida jog
          Mapa (pi , dir) pf blocos = m


-- 5.
colecionavelPers :: [(Colecionavel,Posicao)] ->  Personagem -> Personagem
colecionavelPers [] pers = pers
colecionavelPers ((colec,posC):t) pers
    |hitboxColisao hitboxP  hitboxC && colec == Machado = colecionavelPers t (pers {aplicaDano = (True,600)} )
    |hitboxColisao hitboxP  hitboxC && colec == Cerveja = colecionavelPers t (pers {pontos = (p+10)} )
    |otherwise = colecionavelPers t pers
    where hitboxP = createHitbox (posicao pers) (tamanho pers)
          hitboxC = createHitbox posC (1,1)
          p       = pontos pers
          (a,b)   = aplicaDano pers

colecionavelF :: [(Colecionavel,Posicao)] -> Personagem -> [(Colecionavel,Posicao)]
colecionavelF [] pers = []
colecionavelF ((colec,posC):t) pers
    |hitboxColisao hitboxC  hitboxP && colec == Machado = if fst(aplicaDano pers)== True then colecionavelF t pers else (colec,posC): colecionavelF t pers
    |hitboxColisao hitboxC  hitboxP = colecionavelF t pers
    |otherwise = (colec,posC): colecionavelF t pers
    where hitboxP = createHitbox (posicao pers) (tamanho pers)
          hitboxC = createHitbox posC (0.8,0.8)

-- 6. 
alcapao:: Mapa -> [Personagem] -> Mapa 
alcapao mapa [] = mapa
alcapao (Mapa (a,b) e blocos) (h:t) = case direcao h of
                                    Este  ->   if tipo h == Jogador && buscaElementoCartesiano blocos (x-1,y-1) == Just Alcapao
                                                then (Mapa (a,b) e (mudarAlcapao blocos (x-1,y-1))) 
                                                else alcapao (Mapa (a,b) e blocos) t
                                    Oeste ->   if tipo h == Jogador && buscaElementoCartesiano blocos (x+1,y-1) == Just Alcapao
                                                then (Mapa (a,b) e (mudarAlcapao blocos (x+1,y-1))) 
                                                else alcapao (Mapa (a,b) e blocos) t         
                                    _    -> (Mapa (a,b) e blocos)
                                    where (x,y) = posicao h
                                          comprimento = snd(tamanho h)
                                          

mudarAlcapao::[[Bloco]] -> Posicao -> [[Bloco]]
mudarAlcapao blocos (x,y) = (take (((div (snd tamanhoJanela) 2)-(floor y))-1) blocos)++ (mudarAlcapaoAux (blocos !! (((div (snd tamanhoJanela) 2)-(floor y))-1)) ((div (fst tamanhoJanela) 2)+(floor x))) : drop (((div (snd tamanhoJanela) 2)-(floor y))) blocos

mudarAlcapaoAux :: [Bloco] -> Int -> [Bloco]
mudarAlcapaoAux [] _ = []
mudarAlcapaoAux l c
    |c == 0 = [Vazio] ++ t
    |otherwise = (take (c) l) ++ [Vazio] ++ (drop (c+1) l)
    where (h:t) = l

-- 7. Já está definida em tarefa 1
colisaoJ:: Mapa -> Personagem -> Personagem
colisaoJ mapa pers= if colisoesParede mapa pers && (posicao pers) /= (x,fromInteger(floor y)+0.5)
                    then pers{posicao = (x,fromInteger(floor y)+0.5), velocidade= (vx,0)}
                    else if colisoesParede mapa pers 
                         then case direcao pers of 
                              Este  -> if x >=  9.5 then pers{posicao= (fromInteger(floor x)+0.4, y) , velocidade= (0,vy)} else pers{posicao= (fromInteger(floor x)+0.4, fromInteger(floor y)+0.5), velocidade= (0,vy)}
                              Oeste -> if x <= -9.5 then pers{posicao= (fromInteger(floor x)+0.6, y) , velocidade= (0,vy)} else pers{posicao= (fromInteger(floor x)+0.6, fromInteger(floor y)+0.5), velocidade= (0,vy)}
                              Norte -> pers{velocidade= (vx,0)}
                              Sul   -> pers{velocidade= (vx,0)} 
                         else h
    where h= movimentaAuxP mapa pers
          (x,y)   = posicao pers
          (vx,vy) = velocidade pers


colisaoI:: Semente -> Mapa -> [Personagem] -> [Personagem]
colisaoI _ _ [] = []
colisaoI sem mapa (h:t) = if colisoesParede mapa h || naoCair mapa h then ((movimentaAuxE sem (blocosM mapa) [h{direcao = x, velocidade=(-vx,vy)}]) ++ colisaoI sem mapa t) else ((movimentaAuxE sem (blocosM mapa) [h]) ++ colisaoI sem mapa t)
    where (vx,vy) = velocidade h
          x = case direcao h of             
              Este ->  Oeste
              Oeste -> Este

naoCair:: Mapa -> Personagem -> Bool
naoCair mapa h = buscaElementoCartesiano blocos (x,y-1) == Just Plataforma && (buscaElementoCartesiano blocos (x-1,y-1) == Just Vazio || buscaElementoCartesiano blocos (x+1,y-1) == Just Vazio)
    where (x,y) = posicao h
          blocos = blocosM mapa