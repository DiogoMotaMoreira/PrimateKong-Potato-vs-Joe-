# 🐵 PrimateKong: Potato vs Joe

🎮 Projeto académico desenvolvido na Universidade do Minho, no âmbito da unidade curricular de Laboratórios de Informática I (LI1).

---

## 📌 Descrição

**Potato vs Joe** é uma recriação do clássico jogo **Donkey Kong**, feita em **Haskell**, com foco em aplicar conceitos de programação funcional.  
O objetivo do projeto era replicar a jogabilidade e estrutura de Donkey Kong, adaptando-a ao paradigma funcional.

> ✅ O projeto foi concluído com sucesso, embora alguns tópicos solicitados pelos docentes não tenham sido totalmente implementados.

---

## 🚀 Como Executar

### 🔧 Compilar e Executar

Compile e execute o jogo com:
#### Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do `cabal`.

```bash
cabal run primate-kong
```

#### Interpretador

Pode abrir o interpretador do Haskell (GHCi) utilizando o cabal com o projecto automaticamente carregado.

```bash
cabal repl
```

#### Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Pode correr os testes utilizando o seguinte comando.

```bash
cabal test
```

Se pretender executar os exemplos da documentação como testes unitários utiliza-se a biblioteca [Doctest](https://hackage.haskell.org/package/doctest).

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest
```

#### Documentação

Pode gerar a documentação com o [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```
