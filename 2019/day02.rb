input = File.read("./02/input.txt").chomp.split(',').map(&:to_i)

def cpu(mem, pos)
  case mem[pos]
  when 1
    # add
    mem[mem[pos+3]] = mem[mem[pos+1]] + mem[mem[pos+2]]
  when 2
    # mult
    mem[mem[pos+3]] = mem[mem[pos+1]] * mem[mem[pos+2]]
  when 99
    # halt and output
    return mem[0]
  end
  # run next instruction
  cpu(mem, pos+4)
end

def solve1(input)
  input.dup.then do |mem|
    mem[1] = 12
    mem[2] = 2
    cpu(mem, 0)
  end
end

def solve2(input)
  noun, verb = (0..99).to_a.combination(2).detect do |test|
    19690720 == input.dup.then do |mem|
      mem[1] = test[0]
      mem[2] = test[1]
      cpu(mem, 0)
    end
  end

  100 * noun + verb
end

puts "Part 1: #{ solve1(input) }"
puts "Part 2: #{ solve2(input) }"
