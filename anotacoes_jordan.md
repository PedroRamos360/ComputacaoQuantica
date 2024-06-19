# Arquivo main.hs

É um programa Haskell que implementa operações básicas de computação quântica. Ele define duas estruturas de dados, `Complex` e `Qubit`, e duas funções, `hadamard` e `cnot`.

1. `Complex`: Esta é uma estrutura de dados para representar um número complexo. Um número complexo é um número que pode ser expresso na forma a + bi, onde 'a' e 'b' são números reais e 'i' é a unidade imaginária.

2. `Qubit`: Esta é uma estrutura de dados para representar um qubit, a unidade básica de informação quântica. Um qubit é representado por dois números complexos.

3. `hadamard`: Esta é uma função que implementa a operação de porta de Hadamard em um qubit. A porta de Hadamard é uma operação fundamental na computação quântica que permite a superposição de estados. Ela recebe um `Qubit` como entrada e retorna um novo `Qubit` como resultado.

4. `cnot`: Esta é uma função que implementa a operação de porta CNOT (Controlled NOT) em dois qubits. A porta CNOT é uma operação quântica que inverte o segundo qubit (qubit de destino) se o primeiro qubit (qubit de controle) for 1. Ela recebe dois `Qubit` como entrada e retorna um par de `Qubit` como resultado.

# teleportation.hs

Implementa o protocolo de teletransporte quântico. A função createEntangledPair é uma parte crucial deste protocolo.

createEntangledPair: Esta função cria um par de qubits emaranhados. O emaranhamento é um fenômeno quântico onde dois ou mais qubits se tornam correlacionados de tal maneira que o estado de um qubit está diretamente relacionado ao estado do outro, não importa quão longe eles estejam um do outro. Esta função retorna um par de Qubit.
O protocolo de teletransporte quântico é um método de mover qubits de um local para outro, sem ter que mover a partícula física em si. Ele faz isso usando o emaranhamento quântico e a medição quântica. A função createEntangledPair é usada para criar o par de qubits emaranhados que são necessários para este protocolo.
