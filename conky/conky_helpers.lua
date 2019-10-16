-- conky_helpers.lua
-- author: Seong Yong-ju <sei40kr@gmail.com>

cpu = tonumber(conky_parse('${cpu cpu0}'))
memperc = tonumber(conky_parse('$memperc'))

function conky_divider()
  return '^fg(#3f444a)|^fg()'
end


function conky_cpu_color()
  if 80 <= cpu then
    return '#ff6c6b'
  end

  return ''
end

function conky_mem_color()
  if 90 <= memperc then
    return '#ff6c6b'
  end

  return ''
end
