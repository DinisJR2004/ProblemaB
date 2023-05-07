(****************************************************************************************************)
(*                                                                                                  *)
(*                                        Programação Funcional                                     *)
(*                                             Problema B                                           *)
(*                                                                                                  *)
(*                                                                                                  *)
(*     Realizado por:   Dinis Ramos     - nº 49471                                                  *)
(*                      Ricardo Andrade - nº 49653                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)

let () =
(*leitura de um número inteiro 'n'*)
  let n = read_int() in
    (*Se o valor introduzido for maior de 10000 ou menor que 0 
    o programa termina, caso contrário continua para a linha seguinte*)
    if n <= 10000 && n > 0 then
      (*leitura de um inteiro 'm'*) 
      let m = read_int() in
        (*Se o valor introduzido for menor ou igual que 0 ou maior que 'n' 
        o programa termina, caso contrário continua para a linha seguinte*)
        if m > 0 && m <= n then 
          (*Criação de um um array chamado "precos" de tamanho m, que armazenará o tamanho e o preço*)
          let precos = Array.make m (0, 0) in
          (*Criação de um array chamado "lucro_max" de tamanho 'n+1', que armazenará o lucro máximo para cada tamanho*)
          let lucro_max = Array.make (n + 1) 0 in
    
          let rec read_precos k =
            if k < m then
              let tamanho, preco = Scanf.scanf "%d %d\n" (fun x y -> x, y) in
              precos.(k) <- (tamanho, preco);
              read_precos (k + 1)
            else ()
          in
          (* Função que vai calcular o lucro máximo para um tamanho "tam" *)
          let rec max_lucro tam =
            (* Verifica se "tam" é menor ou igual a "n" *)
            if tam <= n then
              (* Verifica se o lucro máximo para "tam" já foi calculado e se também foi armazenado 
              O "." é usado para aceder ao valor do array em uma determinada posição *)
              if lucro_max.(tam) != 0 then
                (* Acede ao valor do array "lucro_max" na posição "tam" *)
                lucro_max.(tam)
              else
                (* Declaração de uma função auxiliar que vai calcular o lucro máximo *)
                let rec max_lucro_aux i max_aux =
                  (* Se "i" for maior que "m" vai retornar o valor máximo atual 'max_aux' *)
                  if i > m then
                    max_aux
                  else 
                    (* Obtém o tamanho e preço do produto atual *)
                    let tamanho, preco = precos.(i - 1) in
                    (* Verifica se o tamanho do produto é menor ou igual a "tam" *)
                    if tamanho <= tam then
                      (* A função lucro vai buscar o valor das variaveis "preco" e "max_lucro" para calcular o lucro total *)
                      let lucro = preco + max_lucro (tam - tamanho) in
                      max_lucro_aux (i + 1) (max max_aux lucro)
                    else
                      max_lucro_aux (i + 1) max_aux
                in
                (* Chama a função auxiliar com "i" igual a 1 e "max_aux" igual a 0 para iniciar o cálculo do lucro máximo *)
                let resultado = max_lucro_aux 1 0 in
                (*Armazena o resultado no array "lucro_max" na posição "tam" *)
                lucro_max.(tam) <- resultado;
                resultado
            (* Se "tam" for maior que "n" retorna 0 *)
              else 0
              
          in
          (*Chamada da função "read_precos" com o argumento 0, o que faz com que a função
         comece a ler as entradas de preço e tamanho a partir do índice 0 do array "precos"*)
          read_precos 0;
          (*Imprime o resultado da função "max_lucro" para o valor de entrada 'n'*)
          Printf.printf "%d\n" (max_lucro n)



(*
   
          Fontes:

          https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/
          https://stackoverflow.com/questions/1412668/does-have-meaning-in-ocaml



*)
