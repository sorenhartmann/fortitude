program mod1
  
  use mod, only: abc, def

  use mod, only: abc, def, aaa, bbb, ccc => as, ddd, eee, ffff, gggg, hhh, aaaaa, ccccc, ddddd

contains

  subroutine return_val(abc)
      integer, intent(out) :: abc

      abc = 0
      call some_other(abc, abc, abc, abc, abc, f(3, 1234, abc), abc, abc, abc, abc, abc, abc, abc, abc, abc, abc, abc)

      call some_other(abc, 1 + 2 / 2, abc)

      variable = foo + bar * (thing + (jaff + zang) - f(3, 1234, abc) * yeet) + g(huff + bar * (thing + (jaff + zang) - yeet) + (huff + bar * (thing + (jaff + zang) - yeet) + huff + bar * (thing + (jaff + zang) - yeet) + huff)) * 10

      call some_other( 
         1234, abc, & ! A comment about second arg
         1 + 2 / 2, & ! Another comment out last arg
      )

      valid = valid &
         .and. sum(A%values()) &
            + sum(B%values()) &
            + sum(S%values()) == sum(node_ids%values())

      valid = sum(A%values()) &
         + sum(B%values()) &
         + sum(S%values()) == sum(node_ids%values()) &
         .and. is_valid

      a = 1 + 2 / 3
      a = 1 / 2 + 3
      return

  end subroutine

end program mod1
