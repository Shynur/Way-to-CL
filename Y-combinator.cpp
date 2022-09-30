#include <functional>
template<typename return_t, typename ...arguments_t>struct Y {
    using recursive_function_t = std::function<return_t(arguments_t...)>;
    template<typename F>recursive_function_t operator()(const F& f) const {
        static const struct G {
            const F& f;
            recursive_function_t operator()(const G& g) const {
                static const auto gg = [&g](arguments_t ...args) ->return_t {
                    static const auto fn = g(g);
                    return fn(args...);
                };
                return f(gg);
            }
        } g{f};
        return g(g);
    }
};

#include <iostream>
int main() {
    auto fib = Y<size_t, size_t>{}([](const auto& f) {
        return [&f](size_t n) {
            return n <= 1 ? n : (f(n - 1) + f(n - 2));
        };
    });
    
    for (auto i{0}; i != 10; ++i)
        std::cout << fib(i) << std::endl;
}
