snacks = File.readlines('./01/input.txt')
  .slice_before("\n")
  .map do |xs|
    xs.drop(1)
      .map(&:to_i)
      .sum
  end

puts "Part 1: #{snacks.max}"
puts "Part 2: #{snacks.sort.last(3).sum}"
