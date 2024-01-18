{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Diogo Mota Moreira <a106841@alunos.uminho.pt>
              Nuno Gil de Magalhães Baldaia Mendes <a106875@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

--xp -> Posição de personagem 
--xe -> POsição de estrela 


-- Trocar dimensões quando houver uma janela
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) pers =
    case direcao pers of
        Norte -> if (y+0.5)< 9.5    then buscaElementoCartesiano blocos (x,y+0.5)  == Just Plataforma else y >= 9.5   
        Sul   -> if (y-0.5)> (-9.5) then buscaElementoCartesiano blocos (x,y-0.5)  == Just Plataforma else y <= (-9.5 )    -- Trocar medidas para dimensões da tela  -- Estas dimensões são para o mapa de exemplo que é 10 por 10 e janela não está definida
        Este  -> if (x+0.5)< 9.5    then buscaElementoCartesiano blocos (x+0.5,y)  == Just Plataforma || buscaElementoCartesiano blocos (x,y-0.4)  == Just Plataforma || buscaElementoCartesiano blocos (x,y+0.4)  == Just Plataforma else x >= 9.5      -- Trocar medidas para dimensões da tela     
        Oeste -> if (x-0.5)> (-9.5) then buscaElementoCartesiano blocos (x-0.5,y)  == Just Plataforma || buscaElementoCartesiano blocos (x,y-0.4)  == Just Plataforma || buscaElementoCartesiano blocos (x,y+0.4)  == Just Plataforma else x <= (-9.5 )   -- Trocar medidas para dimensões da tela 
    where (x,y)       = posicao pers


-- Função para buscar o elemento a partir de uma coordenada cartesiana
buscaElementoCartesiano :: [[Bloco]] -> Posicao -> Maybe Bloco
buscaElementoCartesiano [[]] _ = Nothing
buscaElementoCartesiano blocos (x,y) = buscarLinha blocos (x,y)

buscarLinha :: [[Bloco]] -> Posicao -> Maybe Bloco
buscarLinha blocos (x,y) 
    |y > 10 || y < (-10) = Nothing
    |otherwise = buscarColuna (blocos !! (((div (snd tamanhoJanela) 2)-(floor y))-1)) (x,y) 

buscarColuna ::[Bloco] -> Posicao -> Maybe Bloco
buscarColuna blocos (x,y)
    |x > 10 || x < (-10) = Nothing
    |otherwise = Just (blocos !! ((div (fst tamanhoJanela) 2)+(floor x)))



colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens pers1 pers2 = hitboxColisao hitbox1 hitbox2
    where hitbox1 = createHitbox (posicao pers1) (tamanho pers1)
          hitbox2 = createHitbox (posicao pers2) (tamanho pers2)

hitboxColisao :: Hitbox -> Hitbox -> Bool 
hitboxColisao ((x1,y1),(x1',y1')) ((x2,y2),(x2',y2'))
    |x2<=x1' && x2'>= x1 && y2<=y1' && y2'>= y1  = True
    |otherwise = False
