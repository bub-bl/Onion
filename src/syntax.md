## Reserved Keywords
> `event`\
> Related to events

> `if`\
> Related to events

> `else`\
> Related to events

> `return`\
> Related to events

> `switch`\
> Related to events

> `case`\
> Related to events

> `break`\
> Related to events

> `skip`\
> Related to events

> `component` Related to component\
> Related to components

> `page` Related to component\
> Related to components

> `layout` Related to component\
> Related to components

> `modifier` Related to variables, functions..\
> Related to events

> `enum` Related to variables, functions..\
> Related to events

> `struct` Related to variables, functions..\
> Related to events

> `override` Related to variables, functions..\
> Related to events

> `base` Related to variables, functions..\
> Related to events

## Reserved Modifiers

> `lazy`\
> Delays the initialization or computation of the variable until it is accessed

> `bind`\
> Establishes a dynamic binding for the variable

> `mut`\
> Allows mutation or modification of the variable

> `required`\
> Specifies that the variable must have a mandatory value

> `persist`\
> Enables persistence or long-term storage for the variable

> `debounce`\
> Applies a stabilization delay or debouncing mechanism to the variable

> `async`\
> Indicates the variable is designed for handling asynchronous operations

> `null`\
> Allow null values

> `cascading`\
> Facilitates broadcasting of the variable across multiple entities

> `sync`\
> Synchronizes or coordinates the variable with other elements

> `telemetry`\
> Incorporates the variable into telemetry events or data collection for performance monitoring

> `deprecated`\
> Marks the variable as associated with deprecated or discouraged elements

> `get`
> 

> `set`
> 

> `is`
>

> `as`
>

## Reserved Modifier Contraints
> `min=`\
> Imposes a minimum value constraint on the variable

> `max=`\
> Imposes a maximum value constraint on the variable

> `pattern=`\
> Enforces a matching pattern or regular expression for the variable

> `format=`\
> Imposes a format for the variable value

## Examples
### Advanced Component
```rs
// File: ./shared/entities/SharedEntityData.onion
decl SharedEntityData: [pub, interface] {
    let id: [pub];
}

// File: ./components/User.onion
import SharedEntityData in './shared/entities';

decl UserData: [pub, impl=SharedEntityData] {
    let firstname: [pub, bind, init];
    let lastname;

    fn new(firstname: "John", lastname: "Doe"): [pub, static] => UserData {
        firstname,
        lastname,
    }

    fn set_age(age: [min=0, max=100]): [pub, virtual] {
        self.age = age;
    }
}

decl DefaultLayout: [pub, impl=Layout] {
    let title: [pub, bind] = "Default Layout";

    fn on_mount(): [override] {
        cascade("my_title", self.title);
    }

    fn render(): [override] => Container {
        Header {
            title: "Title: {title}";
        }
        Body {..}
    }
}

decl Home: [pub, impl=Page, route=["/", ], authorize=["owner", "admin"], layout=PageLayout] {
    fn on_mount(): [override] {
        layout.title = "Home";
    }

    fn render(): [override] => Container {
        background: black;

        content: Text {
            content: "Home Page!";
            color: rgb(0, 255, 0);
        };
    }
}

decl User: [pub, impl=[UserData, Component]] {
    let title: [cascade="my_title"];

    fn set_data(data: [impl=Any]): [pub] {
        self.data = data;
    }

    fn set_age(age: [mut, min=0, max=75] = 5): [override] {
        base.set_age(age);
        self.data.set_age(age);

        age += 10;
        self.age = age;
    }

    // Called when the component is created
    fn on_mount(): [override] {
        base.on_mount();
        println("User mounted");
    }

    // Builtin function
    // Called when the component is destroyed
    fn on_dismount(): [override] {
        println("User dismounted");
    }

    // Builtin function
    // Called when the component is rendered
    fn render(): [override] => if parent_page {
        parent_page.header_title = "User: {firstname}";
    } else Container {
        Text {
            content: firstname;

            styles: Style {
                color: rgb(255, 255, 255),
            };
        }
    }
}
```

```rs
// Default variable
let age = 26;
let age: [const] = 26;
let age: [bind, init, u8] = 26;
```