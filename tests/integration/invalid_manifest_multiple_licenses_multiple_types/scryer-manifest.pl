name("name_of_the_package").
main_file("main.pl").
license(name("UNLICENSE")).
license(name("UNLICENSE"), path("./license")).
dependencies([
    dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git"))
]).
