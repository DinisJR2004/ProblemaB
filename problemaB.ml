let () =
  let n = read_int() in
  if n <= 10000 && n > 0 then 
    let m = read_int() in
    if m > 0 && m <= n then 
      let precos = Array.make m (0, 0) in
      let lucro_max = Array.make (n + 1) 0 in

      let rec read_precos tabela =
        if tabela < m then
          let tamanho, preco = Scanf.scanf "%d %d\n" (fun x y -> x, y) in
          precos.(tabela) <- (tamanho, preco);
          read_precos (tabela + 1)
        else ()
      in

      let rec max_lucro tam =
        if tam <= n then
          if lucro_max.(tam) <> 0 then
            lucro_max.(tam)
          else
            let rec max_lucro_aux i max_aux =
              if i > m then
                max_aux
              else 
                let tamanho, preco = precos.(i - 1) in
                if tamanho <= tam then
                  let lucro = preco + max_lucro (tam - tamanho) in
                  max_lucro_aux (i + 1) (max max_aux lucro)
                else
                  max_lucro_aux (i + 1) max_aux
            in
            let resulto = max_lucro_aux 1 0 in
            lucro_max.(tam) <- resulto;
            resulto
        else
          0
      in

      read_precos 0;
      Printf.printf "%d\n" (max_lucro n)




(*
   
      Fontes:


      https://stackoverflow.com/questions/1412668/does-have-meaning-in-ocaml














*)
