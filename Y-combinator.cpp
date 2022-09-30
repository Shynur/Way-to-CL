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
#include <utility>
template<typename return_t, typename ...arguments_t> auto y = [](const auto& f) {
    return [](const auto& g) {return g(g);} ([&f](const auto& x) ->std::function<return_t(arguments_t...)> {
        return f([&x](arguments_t ...args) {return x(x)(args...);});
    });
};

#include <iostream>
int main() {
    auto fib = y<size_t, uint8_t, size_t, size_t>([](const auto& f) {
        return [&f](uint8_t n, size_t a, size_t b) {
            return n == 0 ? a : f(--n, b, a + b);
        };
    });

    for (auto i{0}; i != 10; ++i)
        std::cout << fib(i, 0, 1) << std::endl;
}
