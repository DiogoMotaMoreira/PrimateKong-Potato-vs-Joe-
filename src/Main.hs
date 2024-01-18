module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import LI12324
import Tarefa1 
import Tarefa2
import Tarefa3
import Tarefa4
import Jogos
import DesenhaPrimate

-- 1 -- função que cria um estado inicial ---------------------------------------------------------
pk :: PrimateKong 
pk = PrimateKong {
            jogo = jogo4,
            menu = MenuInicial,
            tempo = 0,
            tempoImg = 0,
            opcao = Jogar,
            imagens = Imagens{
                           blocos = [],
                           personagens = [],
                           menus = [],
                           colec = []}
            }

----------------------------------- Imagens ---------------------------------------

inicio:: ([(Bloco,Picture)],[(Entidade,Picture)],[Picture],[(Colecionavel,Picture)]) -> PrimateKong
inicio l = pk{imagens = Imagens{
                           blocos = (\(a,_,_,_) -> a) l,
                           personagens = (\(_,a,_,_) -> a) l,
                           menus = (\(_,_,a,_) -> a) l,
                           colec = (\(_,_,_,a) -> a) l
                       }}


-- Desenha imagens ---------------------------------------------------------------------------------
desenhaPrimate :: PrimateKong -> Picture 
desenhaPrimate pk = case menu pk of
        MenuInicial -> desenhaMenuInicial pk
        EmJogo      -> desenhaJogo pk 
        MenuVenceu  -> desenhaMV pk
        MenuPerder  -> desenhaMP pk
        Pausa       -> desenhaP pk
        Nivel       -> desenhaNivel pk
        --Definicao   -> desenhaDefinicao pk
        --MenuPausa   -> desenhaMenuPausa pk


-- Reage eventos-------------------------------------------------------------------------------------
reagePrimate :: Event -> PrimateKong -> PrimateKong
reagePrimate event pk = case menu pk of
        MenuInicial -> reageMI event pk
        EmJogo      -> reageJ  event pk
        MenuVenceu  -> reageMVP event pk
        MenuPerder  -> reageMVP event pk
        Pausa       -> reagePausa event pk
        Nivel       -> reageNivel event pk
        --Definicao   -> reageD  event pk          
        --MenuPausa   -> reageMP event pk

reageMI :: Event -> PrimateKong -> PrimateKong
reageMI (EventKey (Char '1') Down _ _) pk = pk{menu= Nivel}
reageMI (EventKey (Char '2') Down _ _) pk = pk{menu= Nivel}
reageMI (EventKey (Char '3') Down _ _) pk = error "" 
reageMI _                              pk = pk

reageNivel :: Event -> PrimateKong -> PrimateKong
reageNivel (EventKey (Char '1') Down _ _) pk = pk{menu= EmJogo, jogo= jogo1}
reageNivel (EventKey (Char '2') Down _ _) pk = pk{menu= EmJogo, jogo= jogo2}
reageNivel (EventKey (Char '3') Down _ _) pk = pk{menu= EmJogo, jogo= jogo3}
reageNivel (EventKey (Char '4') Down _ _) pk = pk{menu= EmJogo, jogo= jogo4}
reageNivel _                              pk = pk


reageJ :: Event -> PrimateKong -> PrimateKong
reageJ (EventKey(Char 'p')            Down _ _) pk = pk{menu = Pausa}
reageJ e pk = pk {jogo = (atualiza (eventI (inimigos(jogo pk))) (eventJ e pk) (jogo pk))}

reageMVP :: Event -> PrimateKong -> PrimateKong
reageMVP (EventKey (Char '1') Down _ _) pk = pk{menu= Nivel}
reageMVP (EventKey (Char '2') Down _ _) pk = pk{menu= MenuInicial}
reageMVP (EventKey (Char '3') Down _ _) pk = error "" 
reageMVP _                              pk = pk

reagePausa :: Event -> PrimateKong -> PrimateKong
reagePausa (EventKey (Char '1') Down _ _) pk = pk{menu= EmJogo}
reagePausa (EventKey (Char '2') Down _ _) pk = pk{menu= MenuInicial}
reagePausa _                              pk = pk

-- Função de tempo -------------------------------------------------------------------------------------
reageTempo:: Float -> PrimateKong -> PrimateKong
reageTempo t pk = case menu pk of
        Pausa       -> pk
        EmJogo      -> pk {jogo = movimenta 2324 (realToFrac t) jogo', tempo = (realToFrac t), tempoImg = ((realToFrac t)+b), menu = if filter (\(a,b) -> a == Estrela) (colecionaveis (jogo pk)) == [] then MenuVenceu else if (vida (jogador (jogo pk)))== 0 then MenuPerder else menu pk}
        MenuInicial -> pk
        _           -> pk
        where b = tempoImg pk
              temp = round ((tempoImg pk)*500)
              (estado , ta) = (aplicaDano (jogador (jogo pk))) 
              jogo' = ((jogo pk) {jogador = jogador'})
              jogador' = if ta > 0 then (jogador (jogo pk)){aplicaDano = (estado ,ta-1)} else (jogador (jogo pk)){aplicaDano = (False ,0)}

-------------------------------------------------------------------------------------------------------------
dm::Display
dm = InWindow
     "PK VS Joe"
     (1000,1000)
     (0,0)

fr:: Int 
fr = 60

main:: IO()
main= do 
        mi <- loadBMP "img/imgMenu/menu.bmp"
        mv <- loadBMP "img/imgMenu/venceu.bmp"
        mp <- loadBMP "img/imgMenu/menuPerder.bmp"
        pausa <- loadBMP "img/imgMenu/pausa.bmp"
        niveis<- loadBMP "img/imgMenu/niveis.bmp"
        joe_a1_d <- loadBMP "img/imgPers/Joe_andar1_d.bmp"
        joe_a1_e <- loadBMP "img/imgPers/Joe_andar1_e.bmp"
        joe_a2_d <- loadBMP "img/imgPers/Joe_andar2_d.bmp"
        joe_a2_e <- loadBMP "img/imgPers/Joe_andar2_e.bmp"
        joe_c1 <- loadBMP "img/imgPers/joe_costas1.bmp"
        joe_c2 <- loadBMP "img/imgPers/joe_costas2.bmp"
        joe_md_a1 <- loadBMP "img/imgPers/joe_armado1_d.bmp"
        joe_me_a1 <- loadBMP "img/imgPers/joe_armado1_e.bmp"
        joe_md_a2 <- loadBMP "img/imgPers/joe_armado2_d.bmp"
        joe_me_a2 <- loadBMP "img/imgPers/joe_armado2_e.bmp"
        joe_m_c1 <- loadBMP "img/imgPers/joe_armadoC1.bmp"
        joe_m_c2 <- loadBMP "img/imgPers/joe_armadoC2.bmp"
        pot_a1_d <- loadBMP "img/imgPers/potato_andar1_d.bmp"
        pot_a1_e <- loadBMP "img/imgPers/potato_andar1_e.bmp"
        pot_a2_d <- loadBMP "img/imgPers/potato_andar2_d.bmp"
        pot_a2_e <- loadBMP "img/imgPers/potato_andar2_e.bmp"
        escada <- loadBMP "img/imgMapa/escada.bmp"
        plataforma <- loadBMP "img/imgMapa/plataforma.bmp"
        alcapao <- loadBMP "img/imgMapa/alcapao.bmp"
        background <- loadBMP "img/imgMapa/bg .bmp"
        machado1 <- loadBMP "img/imgColec/machado1.bmp"
        machado2 <- loadBMP "img/imgColec/machado2.bmp"
        machado3 <- loadBMP "img/imgColec/machado3.bmp"
        machado4 <- loadBMP "img/imgColec/machado4.bmp"
        cerveja1 <- loadBMP "img/imgColec/cerveja1.bmp"
        cerveja2 <- loadBMP "img/imgColec/cerveja2.bmp"
        cerveja3 <- loadBMP "img/imgColec/cerveja3.bmp"
        estrela1 <- loadBMP "img/imgColec/estrela1.bmp" 
        estrela2 <- loadBMP "img/imgColec/estrela2.bmp"
        kong     <- loadBMP "img/imgPers/PotatorKong.bmp"
        play dm              -- janela onde irá correr o jogo
            (greyN 0.5)       -- cor do fundo da janela
            fr                -- frame rate
            (inicio ([(Plataforma, scale 0.03125 0.03125 plataforma), (Alcapao, scale 0.03125 0.03125 alcapao), (Escada, scale 0.03125 0.03125 escada)],
                     [(Jogador, scale 0.05 0.05 joe_a1_d),(Jogador, scale 0.05 0.05 joe_a1_e),(Jogador, scale 0.05 0.05 joe_a2_d),(Jogador, scale 0.05 0.05 joe_a2_e),(Jogador, scale 0.05 0.05 joe_c1),(Jogador, scale 0.05 0.05 joe_c2),(Jogador, scale 0.05 0.05 joe_md_a1),(Jogador, scale 0.05 0.05 joe_me_a1),(Jogador, scale 0.05 0.05 joe_md_a2),(Jogador, scale 0.05 0.05 joe_me_a2),(Jogador, scale 0.05 0.05 joe_m_c1),(Jogador, scale 0.05 0.05 joe_m_c2),(Fantasma, scale 0.05 0.05 pot_a1_d),(Fantasma, scale 0.05 0.05 pot_a1_e),(Fantasma, scale 0.05 0.05 pot_a2_d),(Fantasma, scale 0.05 0.05 pot_a2_e),(MacacoMalvado, scale 0.03125 0.03125 kong)],
                     [mi ,scale 0.061728395 0.061728395 background, scale 0.1 0.1 mv, scale 0.1 0.1 mp, scale 0.05 0.05 pausa , scale 0.05 0.05 niveis],
                     [(Machado,scale 0.03125 0.03125 machado1),(Machado,scale 0.03125 0.03125 machado2),(Machado,scale 0.03125 0.03125 machado3),(Machado,scale 0.03125 0.03125 machado4),(Cerveja,scale 0.05 0.05 cerveja1),(Cerveja,scale 0.05 0.05 cerveja2),(Cerveja,scale 0.05 0.05 cerveja3),(Estrela,scale 0.05 0.05 estrela1),(Estrela,scale 0.05 0.05 estrela2)]))           -- estado inicial
            desenhaPrimate    -- desenha o estado do jogo
            reagePrimate      -- reage a um evento
            reageTempo        -- reage ao passar do tempo

