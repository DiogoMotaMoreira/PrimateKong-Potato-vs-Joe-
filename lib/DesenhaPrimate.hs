module DesenhaPrimate where 

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture



--
desenhaMenuInicial :: PrimateKong -> Picture 
desenhaMenuInicial pk = scale (5) (5) $ (menus (imagens pk)) !! 0

desenhaMV :: PrimateKong -> Picture 
desenhaMV pk = Pictures $ [scale (50) (50) $ (menus (imagens pk)) !! 2] ++ [translate (1) (-400) $ scale 0.3 0.3 $ color black $ text ("Pontos: " ++ show (pontos (jogador (jogo pk))))]

desenhaMP :: PrimateKong -> Picture 
desenhaMP pk = Pictures $ [scale (50) (50) $ (menus (imagens pk)) !! 3] ++ [translate (1) (-400) $ scale 0.3 0.3 $ color black $ text ("Pontos: " ++ show (pontos (jogador (jogo pk))))]

desenhaP :: PrimateKong -> Picture 
desenhaP pk = scale (50) (50) $ (menus (imagens pk)) !! 4

desenhaNivel :: PrimateKong -> Picture 
desenhaNivel pk = scale (50) (50) $ (menus (imagens pk)) !! 5

--
desenhaJogo:: PrimateKong -> Picture
desenhaJogo pk = scale 50 50 $ Pictures ([menus (imagens pk) !! 1] ++ (desenhaMaps (blocos (imagens pk)) m (x+0.5,y-0.5)) ++ [(desenhaJog pk (personagens (imagens pk)))] ++ (desenhaInim pk (inimigos (jogo pk)) (personagens (imagens pk))) ++ [desenhaPontos pk]++ if (filter (\t -> tipo t == MacacoMalvado) (inimigos (jogo pk))) /= [] then [Translate (realToFrac xk) (realToFrac yk) (snd(listaIKONG !!0))]++ (desenhaColec pk (colec (imagens pk))) else []++ (desenhaColec pk (colec (imagens pk))))
        where (x,y) = pontoMatrizInicio
              Mapa pi pf m = mapa (jogo pk) 
              listaIKONG = (filter (\(a,b) -> a == MacacoMalvado) (personagens (imagens pk)))
              (xk,yk) = posicao k
              k = (filter (\t -> tipo t == MacacoMalvado) (inimigos (jogo pk)))!!0
--

desenhaPontos :: PrimateKong -> Picture 
desenhaPontos pk = translate 9 8 $ scale 0.005 0.005 $ color white $ text (show (pontos (jogador (jogo pk))))

desenhaJog :: PrimateKong-> ImagensPersonagens -> Picture
desenhaJog pk ls = if velocidade pers /= (0,0)
                   then if mod te 200 < 100 
                        then Translate (realToFrac x) (realToFrac y) p1
                        else Translate (realToFrac x) (realToFrac y) p2
                   else Translate (realToFrac x) (realToFrac y) p1
        where temp = (tempoImg pk)
              te = round (temp*350)
              pers = jogador(jogo pk)
              (x,y) = posicao pers
              lista = (filter (\(a,b) -> a == Jogador) ls)
              p1 = case direcao pers of
                   Este -> if fst(aplicaDano(jogador(jogo pk))) == True then snd(lista !! 6) else snd(lista !! 0)
                   Oeste-> if fst(aplicaDano(jogador(jogo pk))) == True then snd(lista !! 7) else snd(lista !! 1)
                   _    -> if fst(aplicaDano(jogador(jogo pk))) == True then snd(lista !! 10) else snd(lista !! 4)
              p2 = case direcao pers of
                   Este -> if fst(aplicaDano(jogador(jogo pk))) == True then snd(lista !! 8) else snd(lista !! 2)
                   Oeste-> if fst(aplicaDano(jogador(jogo pk))) == True then snd(lista !! 9) else snd(lista !! 3)
                   _    -> if fst(aplicaDano(jogador(jogo pk))) == True then snd(lista !! 11) else snd(lista !! 5)

desenhaInim :: PrimateKong -> [Personagem] -> ImagensPersonagens -> [Picture] 
desenhaInim _ [] _ = []
desenhaInim pk (h:t) ls = if mod te 200 < 100 
                          then [Translate (realToFrac x) (realToFrac y) p1] ++ desenhaInim pk t ls
                          else [Translate (realToFrac x) (realToFrac y) p2] ++ desenhaInim pk t ls
        where temp = (tempoImg pk)
              lista = (filter (\(a,b) -> a == Fantasma) ls)
              te = round (temp*350)
              (x,y) = posicao h
              p1 = case direcao h of
                   Este -> snd(lista !! 0)
                   Oeste-> snd(lista !! 1)
                   _    -> snd(lista !! 0)
              p2 = case direcao h of
                   Este -> snd(lista !! 2)
                   Oeste-> snd(lista !! 3)
                   _    -> snd(lista !! 0)

desenhaColec :: PrimateKong -> ImagensColec -> [Picture]
desenhaColec _ [] = []
desenhaColec pk ls  
     |elem Machado list && elem Cerveja list && elem Estrela list = [desenhaMachado pk lMachado] ++ [desenhaCerveja pk lCerveja] ++ [desenhaEstrela pk lEstrela]
     |elem Machado list && elem Cerveja list = [desenhaMachado pk lMachado] ++ [desenhaCerveja pk lCerveja]
     |elem Cerveja list && elem Estrela list = [desenhaCerveja pk lCerveja] ++ [desenhaEstrela pk lEstrela]
     |elem Machado list && elem Estrela list = [desenhaMachado pk lMachado] ++ [desenhaEstrela pk lEstrela]
     |elem Cerveja list = [desenhaCerveja pk lCerveja]
     |elem Estrela list = [desenhaEstrela pk lEstrela]
     |elem Machado list = [desenhaMachado pk lMachado]
     |otherwise         = [Blank]
     where lMachado = (filter (\(a,b) -> a == Machado) ls)
           lCerveja = (filter (\(a,b) -> a == Cerveja) ls)
           lEstrela = (filter (\(a,b) -> a == Estrela) ls)
           list = map (\(a,b)-> a) (colecionaveis (jogo pk))
           

desenhaMachado:: PrimateKong -> ImagensColec -> Picture
desenhaMachado _ [] = Blank
desenhaMachado pk ls
     |mod te 200 < 50  = Translate (realToFrac x) (realToFrac y) (snd(ls !! 0))
     |mod te 200 < 100 = Translate (realToFrac x) (realToFrac y) (snd(ls !! 1))
     |mod te 200 < 150 = Translate (realToFrac x) (realToFrac y) (snd(ls !! 2))
     |otherwise        = Translate (realToFrac x) (realToFrac y) (snd(ls !! 3))
     where (x,y) =snd(f!!0)
           f = filter (\(a,b) -> a == Machado) (colecionaveis (jogo pk))
           temp = (tempoImg pk)
           te = round (temp*100)

desenhaCerveja :: PrimateKong -> ImagensColec -> Picture
desenhaCerveja pk ls = Pictures $ map (\(a,b) -> dcAux pk ls b) f
     where f = filter (\(a,b) -> a == Cerveja) (colecionaveis (jogo pk))
           temp = (tempoImg pk)
           te = round (temp*100)

dcAux :: PrimateKong -> ImagensColec -> Posicao -> Picture
dcAux pk ls pos 
     |mod te 200 < 50  = Translate (realToFrac x) (realToFrac y) (snd(ls !! 0))
     |mod te 200 < 100 = Translate (realToFrac x) (realToFrac y) (snd(ls !! 1))
     |mod te 200 < 150 = Translate (realToFrac x) (realToFrac y) (snd(ls !! 2))
     |otherwise        = Translate (realToFrac x) (realToFrac y) (snd(ls !! 1))
     where (x,y) = pos
           temp = (tempoImg pk)
           te = round (temp*100)

desenhaEstrela :: PrimateKong -> ImagensColec -> Picture
desenhaEstrela pk ls
     |mod te 200 < 100  = Translate (realToFrac x) (realToFrac y) (snd(ls !! 0))
     |otherwise         = Translate (realToFrac x) (realToFrac y) (snd(ls !! 1))
     where (x,y) =snd(f!!0)
           f = filter (\(a,b) -> a == Estrela) (colecionaveis (jogo pk))
           temp = (tempoImg pk)
           te = round (temp*400)
           

desenhaMaps:: ImagensBlocos -> [[Bloco]] -> (Float,Float) -> [Picture]
desenhaMaps _ [] _ = []
desenhaMaps i (h:t) (x,y) = desenhaMapsL i h (x,y) ++ desenhaMaps i t (x,y-1)

desenhaMapsL :: ImagensBlocos -> [Bloco] -> (Float,Float) -> [Picture]
desenhaMapsL _ [] _ = []
desenhaMapsL i (h:t) (x,y) = [desenhaMapsAux i h (x,y)] ++ desenhaMapsL i t (x+1,y)
--
desenhaMapsAux :: ImagensBlocos -> Bloco -> (Float,Float) -> Picture
desenhaMapsAux i h (x,y) = case h of 
        Alcapao    -> translate x y (findImgBloco i h)
        Vazio      -> translate x y Blank
        Escada     -> translate x y (findImgBloco i h)
        Plataforma -> translate x y (findImgBloco i h)

findImgBloco:: ImagensBlocos -> Bloco -> Picture
findImgBloco [] _ = Blank
findImgBloco (h:t) bloco
        |fst h == bloco = snd h
        |otherwise = findImgBloco t bloco