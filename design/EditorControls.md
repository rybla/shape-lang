# Editor Controls

## Term Mode

Placement:

| key         | behavior                                      |
| ----------- | --------------------------------------------- |
| `u`         | place U (universe levels not implemented yet) |
| `p`         | place Π (auto-focus on binding)               |
| `l`         | place λ (auto-focus on binding)               |
| `=`         | place let (auto-focus on binding)             |
| `Backspace` | place hole (i.e. dig i.e. delete)             |

Navigation:

| key                      | behavior                                                        |
| ------------------------ | --------------------------------------------------------------- |
| `LeftArrow`, `Shift+Tab` | go left (or, go up, if focus is locally left-most)              |
| `RightArrow`, `Tab`      | go right (or, go up then right, if focus is locally right-most) |
| `UpArrow`                | go up                                                           |
| `DownArrow`              | go down (to left-most subnode)                                  |
| `x`                      | enter binding mode for current focus (only for Π, λ, let)       |

## Binding Mode

| key        | behavior                                     |
| ---------- | -------------------------------------------- |
| char       | append char to binding label                 |
| `Enter`    | return to term mode                          |
| navigation | return to term mode, then perform navigation |
