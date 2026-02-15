# What is Polka?

Polka is a dotfile templatizer/manager that aims to be a drop-in replacement for
GNU Stow with a bespoke scripting language for templating.

Its main goals are 
- No separate template files
- Instant & easy adoption for Stow users
- Deterministic, reproducible template output

### Demonstration

Polka allows you to include directly templates in your dotfiles without worrying
about managing separate template files. Templating code is placed inside
comments that begin with `*`, e.g. for .toml files template code begins with
`#*`.

```toml
# Alacritty.toml

[font]
# Use a different font on different host
#* if sys.hostname == 'laptop' then
family = "Jetbrains Mono"
#* else
family = "MapleMono"
#* end

[colors]
# Configure colorscheme based on external file. The f'...' syntax is a
# python-style format string, and the string will be inserted into the resulting
# file
#* colors = import("settings.polka").colorscheme
#* f'background = "{colors.background}"'
#* f'foreground = "{colors.foreground}"'
```

# Language features

- Expressions produce text
  - `#* "Hello world"` will insert "Hello world" into the file. This can be
    combined with python-style format strings to add dynamic content to your
    files
    ```
    #* font = import("preferences.polka").monospace_font
    #* f"font_family = {font}"
    ```
- Colors are first class values
  - Colors can be constructed from hex code: 
    ```
    #* background = #26272e
    ```
  - Color manipulation is built in:
    ```
    #* foreground = background.lighten(0.4).mix(#3e8a85)
    ```
- System-aware configuration
  - You can use the builtin `sys` value to get information about the system like
    hostname, distro, username, etc.
- Completely deterministic per system
  - Your template code may **not** read/write files, read environment variables,
    run shell commands, or perform network access. This ensures that your
    dotfiles will consistently behave the same on the same system.

### Example 

```
# Types
#* string = "World"
#* formatted_string = f"Hello, {string}!"
#* multiline_string = \\Hello,
#*                    \\World!
#* number = 10.2
#* boolean = true
#* boolean = false
#* nothing = nil
#* color = #AB4309
#* color = Color(0.0, 1.0, 0.5)
#* list = {'a', 'b', 'c', 'd'}
#* dict = {foo = 0, bar = 1, baz = 2}

# Function definition
#* func add(a, b)
#*   result = a + b
#*   return result
#* end

# Codeblocks execute everything inside them without requiring `#*` and are
# delimited by `#**`
#**
export func format_color(color)
  return f'rgba({color.r}, {color.g}, {color.b}, {color.alpha})'
end

colors = {
  red = #FF0000.darken(0.3),
  blue = #00FF00.darken(0.3),
  green = #0000FF.darken(0.3),
}

for color_name, color in colors do
  f'{color_name} = "{format_color(color)}"'
end
#**

# Modify file metadata using the builtin `polka` function
# This will place the generated file in the specified file path
#* polka({ destination = "~/.config/alacritty.toml" })
# The `polka` builtin will also allow you to override filetypes' commentstrings,
# make the templater ignore certain files, and more when called in a
# `config.polka` file
```

# Why Polka over Stow?

Polka is a drop-in replacement for Stow, and you can begin using it the exact
same way you've been using Stow after 
[initializing Polka](#Migration-from-Stow-to-Polka).

However, Polka provides an easy-to-use templating system that can be directly
embedded into your already existing files; there is no need to manage separate
template files. This templating system eliminates the need for having separate
git branches for different machines; you can put your configuration in one file
on one git branch and have if statements to branch depending on what machine
you're on.

Polka will also allow you to override the destination filename and directory of
your files, to allow greater flexibility in managing your dotfiles.


# Migration from Stow to Polka

After initializing Polka with `polka init`, you can immediately replace usage of
`stow .` or `stow *` with `polka sync`. (If you set up your dotfiles repo to use
packages and run `stow *`, you will need to run `polka init
--migrate-stow-packages`). 

Polka will parse all files recursively in the directory you are in when `polka
sync` is ran to check for any template code in them. Files that don't contain
any template will be symlinked to their respective path that stow will usually
place them, and directories that do not contain any templated files will be
symlinked if there is no package conflict in the same way that Stow does.

When Polka detects template code inside of a file, instead of symlinking the
file, it will evaluate the file and place the contents in the destination where
it would normally be symlinked to.

# FAQ

> Does Polka require any state/metadata to run?

Yes. Polka computes & stores hashes of all files it interacts with, both source
files in `~/dotfiles` and their destination files, to determine if anything has
changed in-between invocations of `polka sync`. Polka also maintains a cache of
computed files' `export`ed variables or functions to avoid unnecessarily
reevaluating files.

> What happens if my template errors?

All errors will be immediately reported when they occur, and the destination
file will not be modified.

> What exactly happens when I run `polka sync`?

Running `polka sync` from your dotfiles directory (assuming it's ~/dotfiles)
will
1. Check `~/dotfiles/config.polka` to check configuration for ignored files,
   update filetype comment string table, etc.
2. Scan recursively through the rest of the files
    - If it is a file, first compare the destination file's hash with the hash
      saved on disk in Polka's metadata. If they differ, you will be prompted to
      check if you want to override the file or abort. If they don't differ, it
      will next compare the source file's hash to see if it differs from the one
      saved on disk. If they differ, the file is recomputed and will be
      symlinked or evaluated depending on if there was template code in the
      file. Then, hashes are recomputed for the source & destination files.
    - If it is a directory, check if there is a `config.polka` in the directory
      for directory-specific configuration. Then, iterate recursively over all
      files in the directory. If all the child files have been symlinked, the
      directory itself will get symlinked.
