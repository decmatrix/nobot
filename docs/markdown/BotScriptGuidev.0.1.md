# BOTSCRIPT v.0.1 Alpha

_Author: Bohdan Sokolovskyi [sokol.chemist@gmail.com](mailto:sokol.chemist@gmail.com). Last update: 01.04.2021_

## Chapter 1. Introduction

```textile
<Tom>  Hello User, what is your name ?
<User> My name is Bohdan.
<Tom>  Hello Bohdan. So, what should we do ?
```

And we will learn new language like `BotScript`. `BotScript` is simple declarative programmin language for creation [chat-bot].  In nowadays bots in very important and popular tite for all buisnes pepoples.

## Chapter 2. Overview

Let's see the next exmaple code of bot in `BotScript`,  `example.bs`:

```bs
@codegen "js"
@platform "web"


bot {
    options {
        name: "Tom";
        port: 8082;
        host: "localhost";
        author: "Bohdan Sokolovskyi";
        version: "0.0.1";
    }

    vars {
        user-name: none;
    }

    start from a;

    state-points {
        a: {
            act: on-a;
        }

        b: {
            act: on-b;
        }
    }

    state-actions {
        on-a: {
            if ?input == "Hello" {
                say "Hello User, what is your name?";
                gotov b;
            } else {
                say "Sorry, i don't understad you";
                gotov a;
            }
        }

        on-b: {
            save ?input to user-name;
            say "Hello " user-name " So, what should we do ?";
            gotov a;
        }
    }
}
```





---

[chat-bot]: chat-bot -   




