```haskell
pauliX :: Qubit -> Qubit
pauliX (a, b) = (b, a)

pauliY :: Qubit -> Qubit
pauliY (a, b) = (0 :+ (-realPart a), imagPart b :+ 0)

pauliZ :: Qubit -> Qubit
pauliZ (a, b) = (a, -b)
```

Essas são as operações necessárias para recuperar o qbit de A em C referenciados em
[Conditional operations](conditional_operations.png).

- pauliX: Representa o bit flip, ou state flip (operação X)
- pauliZ: Representa o phase flip (operação Z)
- pauliY: Representa o bit flip combinado com o phase flip (operação Y ou XZ)

```haskell
hadamard :: Qubit -> Qubit
hadamard (a, b) = ((a + b) / sqrt 2, (a - b) / sqrt 2)
```

Define a operação de hadamard que cria a superposição de estados A e B (? não tenho certeza)

```haskell
cnot (a1, b1) (a2, b2) =
    if magnitude b1 == 0 then ((a1, b1), (a2, b2))
    else ((a1, b1), pauliX (a2, b2))
```

Define o gate CNOT que flipa o segundo qbit caso o primeiro seja |1>
Exemplo:
primeiro qbit |1>
cnot (0, 1) (1, 0) => (0, 1) (0, 1)
primeiro qbit |0>
cnot (1, 0) (1, 0) => (0, 1) (1, 0)
Lembrando que a notação |> representa uma matriz vertical em que o primeiro
elemento representa o 0 e o segundo 1 caso tenha 1 no primeiro elemento (o elemento
que representa o 0) então ela fica |0> caso esteja no segundo elemento fica |1>

```haskell
teleportation :: Qubit -> IO Int
teleportation psi = do
    let (aliceQubit, bobQubit) = createEntangledPair
        (aliceQubit', bobQubit') = applyBellMeasurement psi aliceQubit
        bobQubit'' = applyCorrection aliceQubit' bobQubit'
    measure bobQubit''
```

Essa é a função principal que faz a teleportação e é composta de várias outras subfunções
analisando linha por linha:

`teleportation :: Qubit -> IO Int`
Define que a função vai receber 1 Qubit e retornar 1 Int como output

`teleportation psi = do`
Começa a definição da função, vai receber psi (Qubit) e fazer algo

`let (aliceQubit, bobQubit) = createEntangledPair`
Cria o par emaranhado que retornara 2 Qubits, um da Alice e outro do Bob, createEntagledPair não
recebe nenhum argumento

`let (aliceQubit', bobQubit') = applyBellMeasurement psi aliceQubit`
Aqui é usado o `aliceQubit` retornado pela linha anterior e o parametro `psi` recebido na definição
da função para aplicar a medida de bell ao Qubit de alice, essa medida resulta em 2 Qubits: aliceQubit'
e bobQubit'

`bobQubit'' = applyCorrection aliceQubit' bobQubit'`
A medição de Bell gera dois bits clássicos de informação que Alice envia a Bob então aplica uma correção
ao seu qubit `bobQubit'` que retorna o valor `bobQubit''`

`measure bobQubit''`
Por fim o qubit de bob é medido o que retornará um valor inteiro de 0 ou 1

```haskell
createEntangledPair :: (Qubit, Qubit)
createEntangledPair =
    let entangledQubit = hadamard (1 :+ 0, 0 :+ 0)
    in (entangledQubit, snd $ cnot entangledQubit (0 :+ 0, 1 :+ 0))
```

OBS: `let` define um binding para uma expressão que é definida com o `in`

Essa função cria o par emaranhado que será usado posteriormente na teleportação

`createEntangledPair :: (Qubit, Qubit)`
Define que a função não recebe parâmetros e retorna 2 Qubits

`let entangledQubit = hadamard (1 :+ 0, 0 :+ 0)`
Aqui é aplicado a operação de hadamard no Qubit |0> que cria uma superposição de estados entre |0> e |1>
criando um bit emaranhado

`in (entangledQubit, snd $ cnot entangledQubit (0 :+ 0, 1 :+ 0))`
Essa é a expressão de retorno que vai retornar como primeiro Qubit o `entangledQubit` definido na linha anterior
e usa `snd $ ...` para pegar o segundo argumento de retorno da aplicação do gate `cnot` com os parâmetros `entagledQubit`
e `(0 :+ 0, 1 :+ 0)` equivalente ao Qubit |1>
