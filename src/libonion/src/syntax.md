# Langage Design
Onion does not and will never have any inheritance or interface implementation.


## Directives
```css
// Define the route of this component
#route "/home";

#route-guard "";

// Define the layout of this component
#layout "layouts/default_layout";
```

## Keywords:
`bind`: Auto refresh ui when bind keyword is used\
`if`: If condition\
`else`: Else condition\
`event`: Handle user events such as clicks, input changes, etc.
`loop`: An infinite loop\
`break`: Break the current context. Can only be used in (`for`, `loop`) conditions\
`next`: Continue the current context. Can only be used in (`for`, `loop`) conditions\
`use`: Import a namespace \
`for`: Iterate over a list or set of data \
`mut`: Define a variable as mutable \
`init`: \

## Variable definition
```rust
// Define a constant variable
is_big: true;

firstname: init ""; 

// Define a mutable variable
state: mut "useless";
```

## Struct definition
```csharp
struct User {
    firstname: init;
    is_alive: mut true;
    age: 24;
}
```

## Declare a new component called "MyText" with a "value" argument
```
component<Text> MyText(value) {
    text {
        value: text;
        font-size: bind is_big ? 24 : 16;
        visible: bind is_alive;
    }
}
```

## "MyText" component can be used like this
```
main {
    MyText {
        value: "J'adore les bananes !"
    }
}
```

## Some base components
```
layout CustomLayout {
    pub let header_title: "Default Layout";

    render {
        ..
    }
}

page Home {
    // built-in property
    route: "/home"
    layout: CustomLayout {
        header_title: "From home page!"
    }

    let user = User {
        firstname: "Mickael";
    };

    render {
        if user.call() {
            user.render();

            Text {
                content: "Incroyable!";
            }
        }
    }
}

component Text {
    // Publique, rendu mis à jour automatiquement, la valeur doit être définis à l'initialisation du composant
    pub let value: [bind, init] "";

    // Privé, peu être récupérer depuis la fonction "get_age"
    let age: [get, init] "";

    // Privé, peu être défini et récupérer depuis les fonctions "get_job_name" et "set_job_name("new job")"
    let job_name: [get, set] "";

    // Ceci est un composant "render" built-in
    render {
        // Ceci est un composant "text" built-in
        text {
            content: value,
        }
    }
}

component User {
    pub let firsname: [bind, init] "";
    let gender: "1";

    fn call() {
        if firstname == "Mickael" {
            return true;
        } else {
            return false;
        }
    }

    render {
        // Ajoute le composant Text créer précedement
        Text {
            value: "Banana !";
        }
    }
}
```

## Root content of the page
```
main {
    if is_alive {
        top_container("Welcome on board !");
    }
}
```