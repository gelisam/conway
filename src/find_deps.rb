#!/usr/bin/ruby
Dir["src/**/*.hs", "tests/**/*.hs"].each {|filename|
  dependencies = File.readlines(filename).grep(/^import /).map {|line|
    "src/#{line.chomp.split(/[( )]/)[1].gsub(".", "/")}.hs"
  }.select {|f|
    File.exists?(f)
  }
  
  unless dependencies.empty?
    puts "#{filename}: #{dependencies.join(" ")}"
    puts "\ttouch $@"
  end
}
