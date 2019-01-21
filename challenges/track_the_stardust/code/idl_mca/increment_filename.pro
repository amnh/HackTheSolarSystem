;*****************************************************************************
function increment_filename, old_file

    ; Increment the file extension number if it is numeric
    dot = rstrpos(old_file, '.')
    if (dot eq -1) then return, old_file
    if (dot+1 eq strlen(old_file)) then return, old_file

    ext = strmid(old_file, dot+1, 255)
    file = strmid(old_file, 0, dot+1)
    nc = strlen(ext)
    nc = strtrim(string(nc), 2)

    on_ioerror, done

    ext = fix(ext)+1      ; Convert to number, add one, return on error
    ext = string(ext, format='(i'+nc+'.'+nc+')')
    new_file = file + ext
    return, new_file

done: return, old_file
end
