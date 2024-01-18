module Main where

import Test.HUnit
import Tarefa1
import LI12324
import Tarefa2
import Tarefa3

mapaTeste1 :: [[Bloco]]
mapaTeste1 = [
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Escada     , Plataforma , Alcapao    , Plataforma , Plataforma , Plataforma , Vazio      , Vazio      , Plataforma],
    [Vazio      , Escada     , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Escada     , Plataforma , Plataforma , Plataforma],
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Escada     , Vazio      , Vazio      , Vazio     ],
    [Plataforma , Escada     , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma],
    [Plataforma , Escada     , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Alcapao    , Plataforma , Plataforma , Escada     , Plataforma , Alcapao    , Plataforma],
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Escada     , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma]]

mapaTesteAlcapao :: [[Bloco]]
mapaTesteAlcapao = [
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Escada     , Plataforma , Vazio      , Plataforma , Plataforma , Plataforma , Vazio      , Vazio      , Plataforma],
    [Vazio      , Escada     , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Escada     , Plataforma , Plataforma , Plataforma],
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Escada     , Vazio      , Vazio      , Vazio     ],
    [Plataforma , Escada     , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma],
    [Plataforma , Escada     , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Alcapao    , Plataforma , Plataforma , Escada     , Plataforma , Alcapao    , Plataforma],
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Escada     , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma]]


mapaTeste2:: [[Bloco]]
mapaTeste2 = [
    [Plataforma , Alcapao    , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Escada     , Plataforma , Alcapao    , Plataforma , Plataforma , Plataforma , Vazio      , Vazio      , Plataforma],
    [Vazio      , Escada     , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Escada     , Plataforma , Plataforma , Plataforma],
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Escada     , Vazio      , Vazio      , Vazio     ],
    [Plataforma , Escada     , Plataforma , Plataforma , Plataforma , Plataforma , Alcapao    , Plataforma , Plataforma , Plataforma],
    [Plataforma , Escada     , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Alcapao    , Plataforma , Plataforma , Escada     , Plataforma , Alcapao    , Plataforma],
    [Plataforma , Vazio      , Vazio      , Vazio      , Vazio      , Vazio      , Escada     , Vazio      , Vazio      , Plataforma],
    [Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Plataforma , Vazio      , Plataforma , Plataforma]]
    






jogadorT :: Personagem
jogadorT = Personagem
    { velocidade  = (1,1)
    , tipo        = Jogador
    , posicao     = (0,0)
    , direcao     = Este
    , tamanho     = (2, 2)
    , emEscada    = False -- ^ se está numa escada
    , ressalta    = False
    , vida        = 1 -- ^ não negativo
    , pontos      = 0
    , aplicaDano  = (False, 0) -- ^ se está armado e por quanto tempo ainda
    }

inimigoT :: Personagem
inimigoT = Personagem
    { velocidade  = (1,1)
    , tipo        = Fantasma
    , posicao     = (2,0)
    , direcao     = Este
    , tamanho     = (2, 2)
    , emEscada    = False -- ^ se está numa escada
    , ressalta    = True
    , vida        = 1 -- ^ não negativo
    , pontos      = 0
    , aplicaDano  = (True, 1) -- ^ se está armado e por quanto tempo ainda
    }

jogoTeste1 :: Jogo
jogoTeste1 = Jogo {
                   mapa          = (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1)
                  ,inimigos      = [inimigoT{tamanho = (1,1), posicao = (4.5,-0.5)}]
                  ,colecionaveis = [(Machado,(-0.5,2.5))]
                  ,jogador       = jogadorT{tamanho = (1,1), posicao = (7.5,-0.5),velocidade = (0,0)}
                  }    
jogoTeste2 :: Jogo
jogoTeste2 = Jogo {
                   mapa          = (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1)
                  ,inimigos      = [inimigoT{tamanho = (1,1), posicao = (4.5,-0.5)}]
                  ,colecionaveis = [(Machado,(-0.5,2.5))]
                  ,jogador       = jogadorT{posicao = (7.5,-2.5), velocidade = (0.0,-10.0), tamanho = (1,1)}
                  }    



test_tarefa1 = test[
    "teste parede lado direito"  ~: True ~=? colisoesParede (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) jogadorT {tamanho = (1,1), direcao = Este, posicao = (1.5,-8.5)},
    "teste parede lado esquerdo" ~: True ~=? colisoesParede (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) jogadorT {tamanho = (1,1), direcao = Oeste, posicao = (8.5,-8.5)},
    "teste parede em cima"       ~: True ~=? colisoesParede (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) jogadorT {tamanho = (1,1), direcao = Norte, posicao = (0.5,-2.5)},
    "teste parede em baixo"      ~: True ~=? colisoesParede (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) jogadorT {tamanho = (1,1), direcao = Sul, posicao = (0.5,-2.5)},
    "teste para limite de janela"~: True ~=? colisoesParede (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) (Personagem (0,0) Jogador ( 0.5,-2.5) Norte (1,1) False False 10 0 (False,0)),
    "teste jogador inimigo Este"  ~: True ~=? colisoesPersonagens (jogadorT) (inimigoT),
    "teste jogador inimigo Oeste" ~: True ~=? colisoesPersonagens (jogadorT)(inimigoT {posicao = (-2,0)}),
    "teste jogador inimigo Sul"   ~: True ~=? colisoesPersonagens (jogadorT)(inimigoT {posicao = (0,-2)}),
    "teste jogador inimigo Norte" ~: True ~=? colisoesPersonagens (jogadorT)(inimigoT {posicao = (0,2)}) 
    ]


test_tarefa2 = test[
    "teste chao mapa1"           ~: True   ~=? verificaChao (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1),
    "teste chao mapa2"           ~: False  ~=? verificaChao (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste2),
    "teste valida"               ~: True   ~=? valida (Jogo (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{tamanho = (1,1),posicao = (2.5,-0.5)}, inimigoT{tamanho = (1,1), posicao = (2.5,-0.5)}] [] (jogadorT{tamanho = (1,1), posicao = (2.5,-8.5)})),
    "teste ressalta inimigos"    ~: True   ~=? verificaRessaltaInimigos [inimigoT, inimigoT],
    "teste ressalta inimigos2"   ~: False  ~=? verificaRessaltaInimigos [inimigoT{ressalta = False}],
    "teste ressalta jogador"     ~: True   ~=? verificaRessaltaJogador jogadorT,
    "teste ressalta jogador2"    ~: False  ~=? verificaRessaltaJogador jogadorT{ressalta = True},
    "teste numero2 inimigos"     ~: True   ~=? verificaNumMinimoInimigos [inimigoT, inimigoT],
    "teste numero1 inimigos"     ~: False  ~=? verificaNumMinimoInimigos [inimigoT],
    "teste vida 1 fantasma"      ~: True   ~=? verificaVidaInimigos [inimigoT, inimigoT],
    "teste vida 2 fantasma"      ~: False  ~=? verificaVidaInimigos [inimigoT {vida = 2}, inimigoT {vida = 0}],
    "teste escada 1"             ~: True   ~=? (verificaEscadas (Mapa ((1,1),Norte) (1,1) mapaTeste1) 0),
    "teste escada 2"             ~: False  ~=? (verificaEscadas (Mapa ((1,1),Norte) (1,1) mapaTeste2) 0),
    "teste alçapão jogador"      ~: True   ~=? verificaTamanhoPers jogadorT{tamanho = (1, 1)},
    "teste alçapão inimigo"      ~: False  ~=? verificaTamanhoPers jogadorT{tamanho = (2, 2)},
    "teste baseP jogador"        ~: True   ~=? verificaPersVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [jogadorT{posicao = ((2.5,-0.5))}],
    "teste DentroP jogador"      ~: False  ~=? verificaPersVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [jogadorT{posicao = ((2.5,-1.5))}],
    "teste DentroA jogador"      ~: False  ~=? verificaPersVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [jogadorT{posicao = ((3.5,-1.5))}],
    "teste baseP inimigo"        ~: True   ~=? verificaPersVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{posicao = ((2.5,-0.5))}],
    "teste DentroP inimigo"      ~: False  ~=? verificaPersVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{posicao = ((2.5,-1.5))}],
    "teste DentroA inimigo"      ~: False  ~=? verificaPersVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{posicao = ((3.5,-1.5))}],
    "teste baseP colecionavel"   ~: True   ~=? verificaColecVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [(Machado, (2.5, -0.5))],
    "teste DentroP colecionavel" ~: False  ~=? verificaColecVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [(Machado, (2.5, -1.5))],
    "teste DentroA colecionavel" ~: False  ~=? verificaColecVazio (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [(Machado, (3.5, -1.5))]
    ]

test_tarefa3 = test[
    "teste movimenta"             ~: jogoTeste2                                                                    ~=? movimenta 1 3 jogoTeste1,
    "ataque sem arma"             ~: [inimigoT{posicao = (5.5,-0.5), tamanho =(1,1)}]                              ~=? ataqueJogador jogadorT {posicao = (4.5,-0.5), tamanho =(1,1)} [inimigoT{posicao = (5.5,-0.5), tamanho =(1,1)}],
    "ataque falha jogador"        ~: [inimigoT{posicao = (4.5,-0.5), tamanho =(1,1)}]                              ~=? ataqueJogador jogadorT {posicao = (5.5,-0.5), tamanho =(1,1), aplicaDano = (True,5)} [inimigoT{posicao = (4.5,-0.5), tamanho =(1,1)}],
    "ataque jogador"              ~: [inimigoT{posicao = (5.5,-0.5), tamanho =(1,1), vida = 0}]                    ~=? ataqueJogador jogadorT {posicao = (4.5,-0.5), tamanho =(1,1), aplicaDano = (True,5)} [inimigoT{posicao = (5.5,-0.5), tamanho =(1,1)}],
    "teste remove inimigo"        ~: [inimigoT{posicao = (10000,0),tamanho = (1,1), vida = 0 ,velocidade = (0,0)}] ~=? removeInimigo [inimigoT{posicao = (2.5,-0.5),tamanho = (1,1),vida= 0}],
    "teste gravidade jog"         ~: jogadorT{posicao = (7.5,-0.5), tamanho = (1,1),velocidade = (1.0,-9.0)}       ~=? efeitoGravidadeJ (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) 1 jogadorT{posicao = (7.5,-0.5), tamanho = (1,1)},
    "teste gravidade ini"         ~: [inimigoT{posicao = (7.5,-0.5), tamanho = (1,1),velocidade = (1.0,-9.0)}]     ~=? efeitoGravidadeI (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) 1 [inimigoT{posicao = (7.5,-0.5), tamanho = (1,1)}],
    "teste colisao jog/inim"      ~: jogadorT{posicao = (-0.5,2.5), tamanho = (1,1), vida = 0}                     ~=? colisaoJI jogadorT{posicao = (-0.5,2.5), tamanho = (1,1)} [inimigoT{posicao = (-0.5,2.5), tamanho = (1,1)}],
    "teste colecionavel Machado"  ~: jogadorT{posicao = (-0.5,2.5), tamanho = (1,1), aplicaDano = (True, 10)}      ~=? colecionavelPers [(Machado,(-0.5,2.5))] 0 jogadorT{posicao = (-0.5,2.5), tamanho = (1,1)},
    "teste colecionavel moeda"    ~: jogadorT{posicao = (-0.5,2.5), tamanho = (1,1), pontos = 10}                  ~=? colecionavelPers [(Moeda,(-0.5,2.5))] 0 jogadorT{posicao = (-0.5,2.5), tamanho = (1,1)},
    "teste alcapao jogador"       ~: (Mapa ((0.5,-0.5), Este) (3,3) mapaTesteAlcapao)                              ~=? alcapao (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [jogadorT{posicao = (3.5,-0.5), tamanho = (1,1)}],
    "teste alcapao inimigo"       ~: (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1)                                    ~=? alcapao (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{posicao = (3.5,-0.5),tamanho = (1,1)}],
    "teste colisaoj direita"      ~: jogadorT {posicao = (8.5,-2.5), tamanho = (1,1)}                              ~=? colisaoJ (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) jogadorT {posicao = (8.5,-2.5),tamanho = (1,1)},
    "teste colisaoJ esquerda"     ~: jogadorT {posicao = (1.5,-8.5), direcao = Oeste, tamanho = (1,1)}             ~=? colisaoJ (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) jogadorT {posicao = (1.5,-8.5),direcao = Oeste, tamanho = (1,1)},
    "teste colisaoi direita"      ~: [inimigoT{posicao = (8.5,-2.5), direcao = Oeste, tamanho = (1,1)}]            ~=? colisaoI 10 (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{posicao = (8.5,-2.5),tamanho = (1,1)}],
    "teste colisaoi esquerda"     ~: [inimigoT{posicao = (1.5,-8.5), direcao = Este, tamanho = (1,1)}]             ~=? colisaoI 10 (Mapa ((0.5,-0.5), Este) (3,3) mapaTeste1) [inimigoT{posicao = (1.5,-8.5),direcao = Oeste, tamanho = (1,1)}]
    ]

--test_tarefa4 = test[
  --  ]   

main :: IO ()
main = runTestTTAndExit $ test [test_tarefa1, test_tarefa2, test_tarefa3]
