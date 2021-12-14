class Day3

  def initialize
    @input = File.readlines("03/input.txt")
      .map(&:chars).transpose # create a 2d array and rotate it
  end

  def solve1
    @input
      .map { |b| b.tally.invert.minmax }        # count the bits
      .reduce({gam: "", eps: ""}) do |m, arr|
        m[:gam] << arr[1][1]                    # build the new binary strings
        m[:eps] << arr[0][1]                    # one char at a time
        m
      end
      .values.map { |b| b.to_i(2) }.reduce(:*)  # convert to integers and multiply
  end

  def solve2
    @input
      .map { |b| b.tally.invert.minmax }        # count the bits
  end

end

day3 = Day3.new

puts "Part 1: #{day3.solve1}"
puts "Part 2: #{day3.solve2}"
