
in_code_block = false

def is_code_block_line(line)
  line.start_with?('>')
end

def strip_code_block_prefix(line)
  line.delete_prefix('>').strip
end

STDIN.each_with_index do |line|
  if in_code_block
    if is_code_block_line(line)
      puts strip_code_block_prefix(line)
    else
      in_code_block = false
      puts "```"
      puts line
    end
  else
    if line.start_with?("> ")
      in_code_block = true
      puts "```haskell"
      puts strip_code_block_prefix(line)
    else
      puts line
    end
  end
end
