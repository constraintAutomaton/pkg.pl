name("name_of_the_package").
main_file("main.pl").
main_file("main_alt.pl").
license(name("UNLICENSE")).
dependencies([
    dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git"))
]).
