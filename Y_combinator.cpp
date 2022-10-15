#include <functional>

template<typename return_t, typename ...arguments_t>struct Y {
    using recursive_function_t = std::function<return_t(arguments_t...)>;
    template<typename F>recursive_function_t operator()(const F& f) const noexcept {
        static const struct G {
            const F& f;
            recursive_function_t operator()(const G& g) const noexcept {
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

#include <functional>

template<typename ret_t, typename ...args_t>auto y = [](const auto& f) {
    return [](const auto& g) {return g(g);} ([&f](const auto& x)->std::function<ret_t(args_t...)> {
        return f([&x](args_t ...args) {return x(x)(args...);});
    });
};

#include <iostream>

int main() {
    auto fib_1 = Y<size_t, uint8_t, size_t, size_t>{}([](const auto& f) {
        return [&f](uint8_t n, size_t a, size_t b) {
            return n == 0 ? a : f(--n, b, a + b);
        };
    });

    auto fib_2 = y<size_t, uint8_t, size_t, size_t>([](const auto& f) {
        return [&f](uint8_t n, size_t a, size_t b) {
            return n == 0 ? a : f(--n, b, a + b);
        };
    });

    for (auto i{0}; i != 10; ++i)
        std::cout << fib_1(i, 0, 1) << '\t'
                  << fib_2(i, 0, 1) << std::endl;

    auto test = Y<void, int>{}([](auto f) {
        return [f](int n) {
            if (n == 0)
                throw 1;
            else
                return f(--n);
        };
    });

    try {
        test(64);
    } catch (...) {
        std::cout << "\nOk" << std::endl;
    }
}
