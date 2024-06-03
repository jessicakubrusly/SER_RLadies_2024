# Objetos no R
  
## Números

# Podemos usar o R como uma calculadora.

5 + 4
3 * 2
( 1.4 - 2*(3+10.1) )/20

exp(2)
log(10)
sin(pi)
cos(pi)

#E podemos atribuir valores às variáveis locais e realizar operações a partir delas.

x = 5
x + 4
x^2
(2*x - 1)/3
log((2*x - 1)/3)

## Caracteres

a = "oi"
a
b = "com vai?"
b

paste(a,b)
paste(a,", ",b)
paste0(a,", ",b)

## Lógicos

3 < 2
2 == 2.0
2 != 2.0
x >= 0
(3==4) & (2>=1)
(3==3) | (2>=1)

## Vetores

y = c(2,3,4,5,6)
y

texto1 = c("A","AA","ABB")
texto1

logico = c(F,F,T)
logico

x;y
z = c(x,y)
z

texto2 = c("1","2","3","4","5","6")
c(texto1,texto2)
ab = c(a,b)
ab

y
y + 4
z
y + z

texto1
paste(texto1,"0")

logico
logico & TRUE
logico & c(TRUE,FALSE,FALSE)
logico | TRUE
logico | c(TRUE,FALSE,FALSE)


## Funções


nome_da_funcao = function(argumento1,argumento2){
  # aqui colocamos a operacao que queremos realizar com os valores passados 
  # como argumentos de entrada
  saida = (argumento1 + argumento2 + 10)/2
  #o comando return defini o que a funcao retorna
  return(saida)
}
nome_da_funcao(3,4)


# Problema - Investimento

# Suponha que eu tenha R\$1.000,00 para investirna poupança, que rende 0,5% ao mês. Como posso usar o R para saber o montante que terei aplicado depois de 3 anos?
  
valor = 1000
for(i in 1:36){
  valor = valor + valor*0.5/100
}
valor

#E depois de 10 anos?
  
valor = 1000
for(i in 1:120){
  valor = valor + valor*0.5/100
}
valor

#E se o investimento inicial for de R\$900,00 em vez de R\$1.000,00, quanto eu terei depois de 10 anos?
  
valor = 900
for(i in 1:120){
  valor = valor + valor*0.5/100
}
valor

#E se o investimento rendesse 0,6% ao mês, qual o valor guardado depois de 10 anos?
  
valor = 1000
for(i in 1:120){
  valor = valor + valor*0.6/100
}
valor

# função investimento
  
investimento = function(valor,n,taxa){
  for(i in 1:n){
    valor = valor + valor*taxa/100
  }
  return(valor)
}

investimento(1000,36,0.5)
investimento(1000,120,0.5)
investimento(900,120,0.5)
investimento(1000,120,0.6)

#E se quiséssemos saber quantos meses temos que esperar até conseguirmos juntar R\$ 15.000,00 com uma aplicação inicial de R\$ 1.000, parcelas mensais de R\$ 60 e juros de 0,5% ao mês?

valor = 1000
r = 0.5
parcelas = 60
n = 1
while(valor < 15000){
  valor = valor + valor*r/100 + parcelas
  n = n + 1
}
n
valor

#outra opção
valor = 1000
r = 0.5
parcelas = 60
n = 1
repeat{
  valor = valor + valor*r/100 + parcelas
  n = n + 1
  if(valor > 15000){
    break
  }
}
n
valor

# Que tal uma função?
  
meses_para_juntar = function(montante,investimento,parcelas,r){
  n = 1
  valor = investimento
  repeat{
    valor = valor + valor*r/100 + parcelas
    n = n + 1
    if(valor > montante){
      break
    }
  }
  return(c(n,valor))
}

meses_para_juntar(15000,1000,60,0.5)
meses_para_juntar(20000,1200,100,0.5)
meses_para_juntar(15000,1000,60,0.5)




# Problema - Crescimento Populacional

# Queremos simular o crescimento de uma população de microorganismos dentro de um laboratório.
#-   Chamamos de taxa de nascimento semanal o quanto a população cresce, em porcentagem, a cada semana.
#-   Chamamos de taxa de mortalidade semanal o quanto a população diminui, em porcentagem, a cada semana.
# Suponha uma população inicial de 100 microorganismos com taxa de nascimento de 20% e taxa de morte de 5%. Depois de 4 semanas, qual o tamanho da população?
  
pop = 1000
for(i in 1:4){
  pop = pop + 0.2*pop - 0.05*pop
}
pop

#Depois de 8 semanas, qual o tamanho da população?
  
pop = 1000
for(i in 1:8){
  pop = pop + 0.2*pop - 0.05*pop
}
pop

# Vamos fazer uma função 

populacao = function(pop,nas,mor,n){
  for(i in 1:n){
    pop = pop + nas*pop - mor*pop
  }
  return(pop)
}

populacao(1000,0.2,0.05,20)
populacao(1000,0.2,0.2,20)
populacao(1000,0.05,0.2,20)

#Suponha agora condições limitadas de alimentação.
#-   A mesma taxa de nascimento de 20%.
#-   A taxa de mortalidade que será diferente.
#-   Para população menor que 2.000, a taxa de mortalidade é de 5%.
#-   Para poulação acima de 2.000, a taxa de mortalidade é 5% + (tamanho da populacao - 2000)/10000

#Qual o tamanho dessa populacao depois de 20 dias?
 
for(i in 1:20){
  if(pop < 2000){
    pop = pop + 0.2*pop - 0.05*pop
  } else {
    pop = pop + 0.2*pop - (0.05 + (pop-2000)/10000)*pop
  }
}
pop

# Vamos fazer uma nova função para esse novo cenário.

populacao2 = function(pop,nas,mor,n){
  for(i in 1:n){
    if(pop < 2000){
      pop = pop + nas*pop - mor*pop
    } else {
      pop = pop + 0.2*pop - (0.05 + (pop-2000)/10000)*pop
    }
  }
  return(pop)
}

populacao(1000,0.2,0.05,20)
populacao2(1000,0.2,0.05,20)

populacao(1000,0.2,0.2,20)
populacao2(1000,0.2,0.2,20)

populacao(1000,0.05,0.2,20)
populacao2(1000,0.05,0.2,20)


#Seria interessante a gente observar o crescimento dessa populacao ao longo das semanas.

populacao_vec = function(pop,nas,mor,n){
  vec_pop = pop
  for(i in 1:n){
    pop = pop + nas*pop - mor*pop
    vec_pop = c(vec_pop,pop)
  }
  return(vec_pop)
}

populacao_vec(1000,0.2,0.05,100)

v1 = populacao_vec(1000,0.2,0.05,100)
plot(v1)


populacao2_vec = function(pop,nas,mor,n){
  vec_pop = pop
  for(i in 1:n){
    if(pop < 2000){
      pop = pop + nas*pop - mor*pop
    } else {
      pop = pop + 0.2*pop - (0.05 + (pop-2000)/10000)*pop
    }
    vec_pop = c(vec_pop,pop)
  }
  return(vec_pop)
}


populacao2_vec(1000,0.2,0.05,100)

v2 = populacao2_vec(1000,0.2,0.05,100)
plot(v2)



# Problema - Fatoração

# Como a gente faz para fatorar um número?
# Qual seria a fatoração do número 1234567890 em fatores primos?
#-   Passo 1: O primeiro divisor é $d = 2$.
#-   Passo 2: Se o número for divisível por $d$, divide ele por $d$ e verifica o quociente:
#     -   Se o quiciente for 1, fim.
#     -   Se o quiciente for diferente de 1, repete o passo 2, agora com o quociente.
#-   Passo 3: Se o número não for divisível por $d$, segue.
#-   Passo 4: Faça $d = d + 1$ e volte para o passo 2.

## Problema - Fatoração

options(digits=10)
1234567890/2
quociente = 1234567890/2
quociente
## fatores: 2
quociente/2
quociente/3
quociente = quociente/3
quociente
## fatores: 2, 3
quociente/3
quociente = quociente/3
quociente
## fatores: 2, 3, 3
quociente/3
quociente = quociente/3
quociente
## fatores: 2, 3, 3, 3
quociente/3
quociente/4
quociente/5
quociente = quociente/5
quociente
## fatores: 2, 3, 3, 3, 5
quociente/5
quociente/6
quociente/7
quociente/8
quociente/11
quociente/13
quociente/17
quociente/23
quociente/29
quociente/31
quociente/37
quociente/41
quociente/43
## muito trabalhoso.. vamos fazer um programa que faca isso pra gente?



quociente = 1234567890
fatores = NULL
d = 2
while(quociente>1){
  if(quociente%%d==0){
    quociente = quociente/d
    fatores = c(fatores,d)
  } else {
    d = d + 1
  }
}
fatores


#Vamos fazer uma função que recebe um número qualquer e retorna um vetor com a fatoração deste número em fatores primos?
  
fatoracao  = function(numero){
  quociente = numero
  fatores = NULL
  d = 2
  while(quociente>1){
    if(quociente%%d==0){
      quociente = quociente/d
      fatores = c(fatores,d)
    } else {
      d = d + 1
    }
  }
  return(fatores)
}



fatoracao(123456)
fatoracao(5542132)


#Será que a função `fatoracao` consegue ajudar a gente a definir outra função capaz de identificar se um número é primo ou não?


eh_primo  = function(numero){
  if(numero == 1 || numero == 2){
    return(TRUE)
  }
  d = 2
  while(d<numero){
    if(numero%%d==0){
      return(FALSE)
    } else {
      d = d + 1
    }
  }
  return(TRUE)
}


eh_primo(12345678989)
eh_primo(13)
eh_primo(59)
eh_primo(8317)

# E se quiséssemos uma função que retornasse todos os primos menores que um número dado como argumento de entrada?


primos_menores_que = function(N){
  primos = NULL
  for(i in 1:N){
    if(eh_primo(i)){
      primos = c(primos,i)
    }
  }
  return(primos)
}


primos_menores_que(1000)
primos_menores_que(2000)
