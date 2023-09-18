let string_avoid_132(cs:string): bool =
  let helper1 len1 i =
    if (i < string_length cs) then
      let rec helper2 len2 j =
        if (j < string_length cs) then
          if ord (string_get_at(cs)(i)) < ord (string_get_at(cs)(j)) then
            let rec helper3 len3 k =
              if (k < string_length cs) then
                if ord(string_get_at(cs)(k)) < ord(string_get_at(cs)(j)) && ord(string_get_at(cs)(k)) > ord(string_get_at(cs)(i)) then
                  true
                else
                  helper3 len3 (k+1)
              else
                false
            in
            helper3 (string_length cs - j)(0)
          else
            helper2 len2 (j+1)
        else
          false
      in
      helper2 (string_length cs - i) 0
    else
      false
    
  in
  helper1 (string_length cs) 0;;