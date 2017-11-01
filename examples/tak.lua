function tak(x,y,z)
  if not (y < x) then
    return z
  end
  
  return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
end

print(tak(40,20,11))
