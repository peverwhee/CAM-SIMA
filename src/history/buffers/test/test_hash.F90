module test_hash_utils
   use hist_hashable, only: hist_hashable_char_t

   implicit none
   private

   type, public :: hash_object_t
      type(hist_hashable_char_t), pointer :: item => NULL()
      type(hash_object_t),        pointer :: next => NULL()
   end type hash_object_t

end module test_hash_utils

program test_hash
   use hist_hashable,   only: hist_hashable_t, new_hashable_char
   use hist_hash_table, only: hist_hash_table_t
   use test_hash_utils, only: hash_object_t

   character(len=10) :: hash_names(4) = (/ 'foo       ', 'bar       ',        &
        'foobar    ', 'big daddy ' /)

   type(hash_object_t),    pointer   :: hash_chars => NULL()
   type(hash_object_t),    pointer   :: next_ptr => NULL()
   class(hist_hashable_t), pointer   :: test_ptr => NULL()
   type(hist_hash_table_t)           :: hash_table
   integer                           :: index
   integer                           :: errcnt = 0
   integer,                parameter :: max_errs = 8
   integer,                parameter :: err_size = 128
   character(len=err_size)           :: errors(max_errs)

   errors = ''
   call hash_table%initialize(4)
   do index = 1, size(hash_names, 1)
      if (associated(hash_chars)) then
         allocate(next_ptr%next)
         next_ptr => next_ptr%next
      else
         allocate(hash_chars)
         next_ptr => hash_chars
      end if
      call new_hashable_char(hash_names(index), next_ptr%item)
      call hash_table%add_hash_key(next_ptr%item, errmsg=errors(errcnt + 1))
      if (len_trim(errors(errcnt + 1)) > 0) then
         errcnt = errcnt + 1
      end if
      if (errcnt > max_errs) then
         exit
      end if
   end do

   if (errcnt == 0) then
      ! We have populated the table, let's do some tests
      ! First, make sure we can find existing entries
      do index = 1, size(hash_names, 1)
         test_ptr => hash_table%table_value(hash_names(index),                &
              errmsg=errors(errcnt + 1))
         if (len_trim(errors(errcnt + 1)) > 0) then
            errcnt = errcnt + 1
         end if
         if (errcnt > max_errs) then
            exit
         end if
      end do
      ! Next, make sure we do not find a non-existent entry
      test_ptr => hash_table%table_value(trim(hash_names(1))//'_oops',        &
           errmsg=errors(errcnt + 1))
      if (len_trim(errors(errcnt + 1)) == 0) then
         errcnt = errcnt + 1
         write(errors(errcnt), *) "ERROR: Found an entry for '",              &
              trim(hash_names(1))//'_oops', "'"
      end if
      ! Finally, make sure we get an error if we try to add a duplicate key
      call hash_table%add_hash_key(hash_chars%next%item,                      &
           errmsg=errors(errcnt + 1))
      if (len_trim(errors(errcnt + 1)) == 0) then
         errcnt = errcnt + 1
         write(errors(errcnt), *) "ERROR: Allowed duplicate entry for '",     &
              hash_chars%next%item%key(), "'"
      end if
   end if

   if (errcnt > 0) then
      write(6, '(a,i0,a)') 'FAIL, ', errcnt, ' errors found'
      do index = 1, errcnt
         write(6, *) trim(errors(index))
      end do
      STOP 1
   else
      STOP 0
   end if

end program test_hash
