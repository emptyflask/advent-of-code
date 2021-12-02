input = File.readlines("01/input.txt").map(&:to_i)

class Array
  def window(size)
    return [] unless size > 0 && size <= self.length
    Enumerator.new do |y|
      first, last = 0, size-1
      while first <= self.length - size do
        y << self[first..last]
        first, last = first+1, last+1
      end
    end
  end
end

def solve1(input)
  input.window(2)
    .filter { |a, b| a < b }
    .length
end

def solve2(input)
  input.window(3).map(&:sum).window(2)
    .filter { |a, b| a < b }
    .length
end

puts "Part 1: #{solve1(input)}"
puts "Part 2: #{solve2(input)}"
