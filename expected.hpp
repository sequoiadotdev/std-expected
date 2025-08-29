#ifndef STL_EXPECTED_HPP
#define STL_EXPECTED_HPP

#include <variant>
#include <type_traits>
#include <stdexcept>
#include <utility>
#include <functional>

namespace std {

template<typename E>
class unexpected;

template<typename T, typename E>
class expected;

template<typename T>
struct is_unexpected : std::false_type {};

template<typename E>
struct is_unexpected<unexpected<E>> : std::true_type {};

template<typename T>
inline constexpr bool is_unexpected_v = is_unexpected<T>::value;

class bad_expected_access_base : public std::exception {
public:
    [[nodiscard]] const char* what() const noexcept override {
        return "bad expected access";
    }
};


template<typename E>
class bad_expected_access final : public bad_expected_access_base {
private:
    E error_;
public:
    explicit bad_expected_access( E e )
        : error_( std::move( e ) ) {}

    const E& error() const & noexcept { return error_; }
    E& error() & noexcept { return error_; }
    const E&& error() const && noexcept { return std::move( error_ ); }
    E&& error() && noexcept { return std::move( error_ ); }
};

template<typename E>
class unexpected {
    static_assert( !std::is_same_v<E, void>, "E must not be void" );
    static_assert( !is_unexpected_v<E>, "E must not be unexpected" );
    static_assert( !std::is_reference_v<E>, "E must not be a reference" );
private:
    E error_;
public:
    template<typename Err = E>
    constexpr explicit unexpected( Err&& e )
        noexcept( std::is_nothrow_constructible_v<E, Err> )
        requires ( !std::is_same_v<std::remove_cvref_t<Err>, unexpected> &&
                  !std::is_same_v<std::remove_cvref_t<Err>, std::in_place_t> &&
                  std::is_constructible_v<E, Err> )
        : error_( std::forward<Err>( e ) ) {}

    template<typename... Args>
    constexpr explicit unexpected( std::in_place_t, Args&&... args )
        noexcept( std::is_nothrow_constructible_v<E, Args...> )
        requires std::is_constructible_v<E, Args...>
        : error_( std::forward<Args>( args )... ) {}

    template<typename U, typename... Args>
    constexpr explicit unexpected( std::in_place_t, std::initializer_list<U> il, Args&&... args )
        noexcept( std::is_nothrow_constructible_v<E, std::initializer_list<U>&, Args...> )
        requires std::is_constructible_v<E, std::initializer_list<U>&, Args...>
        : error_( il, std::forward<Args>( args )... ) {}

    unexpected( const unexpected& ) = default;
    constexpr unexpected( unexpected&& ) = default;
    constexpr unexpected& operator=( const unexpected& ) = default;
    constexpr unexpected& operator=( unexpected&& ) = default;

    constexpr ~unexpected() = default;

    constexpr const E& error() const & noexcept { return error_; }
    constexpr E& error() & noexcept { return error_; }
    constexpr const E&& error() const && noexcept { return std::move( error_ ); }
    constexpr E&& error() && noexcept { return std::move( error_ ); }

    constexpr void swap( unexpected& other ) noexcept( std::is_nothrow_swappable_v<E> ) {
        using std::swap;
        swap( error_, other.error_ );
    }

    template<typename E2>
    friend constexpr bool operator==( const unexpected& x, const unexpected<E2>& y ) {
        return x.error_ == y.error();
    }
};

template<typename E>
unexpected(E) -> unexpected<E>;

template<typename T, typename E>
class expected {
    static_assert( !std::is_reference_v<T>, "T must not be a reference" );
    static_assert( !std::is_same_v<T, std::remove_cv_t<std::in_place_t>>, "T must not be in in_place_t" );
    static_assert( !std::is_same_v<T, std::remove_cv_t<unexpected<E>>>, "T must not be a unexpected<E>" );
    static_assert( !is_unexpected_v<T>, "T must not be unexpected" );
private:
    std::variant<T, unexpected<E>> storage_;

    template<typename U, typename G>
    friend class expected;
public:
    using value_type = T;
    using error_type = E;
    using unexpected_type = unexpected<E>;

    constexpr expected()
        noexcept( std::is_nothrow_default_constructible_v<T> )
        requires std::is_default_constructible_v<T>
        : storage_( std::in_place_index<0> ) {}

    expected( const expected& ) = default;
    constexpr expected( expected&& ) = default;

    template<typename U, typename G>
    constexpr explicit( !std::is_convertible_v<const U&, T> || !std::is_convertible_v<const G&, E> )
    expected( const expected<U, G>& other )
        noexcept ( std::is_nothrow_constructible_v<T, const U&> && std::is_nothrow_constructible_v<E, const G&> )
        requires ( std::is_constructible_v<T, const U&> && std::is_constructible_v<E, const G&> &&
                  !std::is_constructible_v<T, expected<U, G>&> &&
                  !std::is_constructible_v<T, expected<U, G>> &&
                  !std::is_constructible_v<T, const expected<U, G>&> &&
                  !std::is_constructible_v<T, const expected<U, G>> &&
                  !std::is_convertible_v<expected<U, G>&, T> &&
                  !std::is_convertible_v<expected<U, G>, T> &&
                  !std::is_convertible_v<const expected<U, G>&, T> &&
                  !std::is_convertible_v<const expected<U, G>, T> )
    {
        if ( other.has_value() ) {
            storage_.template emplace<0>( *other );
        } else {
            storage_.template emplace<1>( other.error() );
        }
    }

    template<typename U, typename G>
    constexpr explicit( !std::is_convertible_v<U, T> || !std::is_convertible_v<G, E> )
    expected( expected<U, G>&& other)
        noexcept( std::is_nothrow_constructible_v<T, U> && std::is_nothrow_constructible_v<E, G> )
        requires ( std::is_constructible_v<T, U> && std::is_constructible_v<E, G> &&
                  !std::is_constructible_v<T, expected<U, G>&> &&
                  !std::is_constructible_v<T, expected<U, G>> &&
                  !std::is_constructible_v<T, const expected<U, G>&> &&
                  !std::is_constructible_v<T, const expected<U, G>> &&
                  !std::is_convertible_v<expected<U, G>&, T> &&
                  !std::is_convertible_v<expected<U, G>, T> &&
                  !std::is_convertible_v<const expected<U, G>&, T> &&
                  !std::is_convertible_v<const expected<U, G>, T> )
    {
        if ( other.has_value() ) {
            storage_.template emplace<0>( std::move( *other ) );
        } else {
            storage_.template emplace<1>( std::move( other ).error() );
        }
    }

    template<typename U = T>
    constexpr explicit( !std::is_convertible_v<U, T> )
    expected( U&& v)
        noexcept( std::is_nothrow_constructible_v<T, U> )
        requires ( !std::is_same_v<std::remove_cvref_t<U>, expected> &&
                   !std::is_same_v<std::remove_cvref_t<U>, std::in_place_t> &&
                   !is_unexpected_v<std::remove_cvref_t<U>> &&
                   std::is_constructible_v<T, U>)
    : storage_( std::in_place_index<0>, std::forward<U>(v) ) {}

    template<typename G = E>
    constexpr explicit( !std::is_convertible_v<const G&, E> )
    expected( const unexpected<G>& e )
        noexcept( std::is_nothrow_constructible_v<E, const G&> )
        requires std::is_constructible_v<E, const G&>
        : storage_( std::in_place_index<1>, e.error() ) {}

    template<typename G = E>
    constexpr explicit( !std::is_convertible_v<G, E> )
    expected( unexpected<G>&& e )
        noexcept( std::is_nothrow_constructible_v<E, G> )
        requires std::is_constructible_v<E, G>
        : storage_( std::in_place_index<1>, std::move( e ).error() ) {}

    template<typename... Args>
    constexpr explicit expected( std::in_place_t, Args&&... args )
        noexcept( std::is_nothrow_constructible_v<T, Args...> )
        requires std::is_constructible_v<T, Args...>
        : storage_( std::in_place_index<0>, std::forward<Args>( args )... ) {}

    template<typename U, typename... Args>
    constexpr explicit expected( std::in_place_t, std::initializer_list<U> il, Args&&... args )
        noexcept( std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...> )
        requires std::is_constructible_v<T, std::initializer_list<U>&, Args...>
        : storage_( std::in_place_index<0>, il, std::forward<Args>( args )... ) {}

    constexpr expected& operator=( const expected& ) = default;
    constexpr expected& operator=( expected&& ) = default;

    constexpr ~expected() = default;

    template<typename U = T>
    constexpr expected& operator=( U&& v )
        requires ( !std::is_same_v<expected, std::remove_cvref_t<U>> &&
                  !is_unexpected_v<std::remove_cvref_t<U>> &&
                  std::is_constructible_v<T, U> &&
                  std::is_assignable_v<T&, U> &&
                  ( std::is_nothrow_constructible_v<T, U> || std::is_nothrow_move_constructible_v<T> ||
                    std::is_nothrow_move_constructible_v<E> ) )
    {
        if ( has_value() ) {
            **this = std::forward<U>( v );
        } else {
            storage_.template emplace<0>( std::forward<U>( v ) );
        }
        return *this;
    }

    template<typename G>
    constexpr expected& operator=( const unexpected<G>& e )
        requires ( std::is_constructible_v<E, const G&> && std::is_assignable_v<E&, const G&> &&
                  ( std::is_nothrow_constructible_v<E, const G&> || std::is_nothrow_move_constructible_v<T> ||
                    std::is_nothrow_move_constructible_v<E> ) )
    {
        if ( has_value() ) {
            storage_.template emplace<1>( e.error() );
        } else {
            error() = e.error();
        }
        return *this;
    }

    template<typename G>
    constexpr expected& operator=( unexpected<G>&& e )
        requires ( std::is_constructible_v<E, G> && std::is_assignable_v<E&, G> &&
                  ( std::is_nothrow_constructible_v<E, G> || std::is_nothrow_move_constructible_v<T> ||
                    std::is_nothrow_move_constructible_v<E> ) )
    {
        if ( has_value() ) {
            storage_.template emplace<1>( std::move( e ).error() );
        } else {
            error() = std::move( e ).error();
        }
        return *this;
    }

    constexpr const T* operator->() const noexcept {
        return &std::get<0>( storage_ );
    }

    constexpr T* operator->() noexcept {
        return &std::get<0>( storage_ );
    }

    constexpr const T& operator*() const& noexcept {
        return std::get<0>( storage_ );
    }

    constexpr T& operator*() & noexcept {
        return std::get<0>( storage_ );
    }

    constexpr const T&& operator*() const&& noexcept {
        return std::get<0>( std::move( storage_ ) );
    }

    constexpr T&& operator*() && noexcept {
        return std::get<0>( std::move( storage_ ) );
    }

    constexpr explicit operator bool() const noexcept {
        return has_value();
    }

    [[nodiscard]] constexpr bool has_value() const noexcept {
        return storage_.index() == 0;
    }

    constexpr const T& value() const& {
        if ( !has_value() ) {
            throw bad_expected_access<E>( std::get<1>( storage_ ).error() );
        }

        return std::get<0>( storage_ );
    }

    constexpr T& value() & {
        if ( !has_value() ) {
            throw bad_expected_access<E>( std::get<1>( storage_ ).error() );
        }
        return std::get<0>( storage_ );
    }

    constexpr const T&& value() const&& {
        if ( !has_value() ) {
            throw bad_expected_access<E>( std::get<1>( std::move( storage_ ) ).error() );
        }
        return std::get<0>( std::move( storage_ ) );
    }

    constexpr T&& value() && {
        if ( !has_value() ) {
            throw bad_expected_access<E>( std::get<1>( std::move( storage_ ) ).error() );
        }
        return std::get<0>( std::move( storage_ ) );
    }

    constexpr const E& error() const& noexcept {
        return std::get<1>( storage_ ).error();
    }

    constexpr E& error() & noexcept {
        return std::get<1>( storage_ ).error();
    }

    constexpr const E&& error() const&& noexcept {
        return std::get<1>( std::move( storage_ ) ).error();
    }

    constexpr E&& error() && noexcept {
        return std::get<1>( std::move( storage_ ) ).error();
    }

    template<typename U>
    constexpr T value_or( U&& default_value ) const&
        requires ( std::is_copy_constructible_v<T> && std::is_convertible_v<U&&, T> )
    {
        return has_value() ? **this : static_cast<T>( std::forward<U>( default_value ) );
    }

    template<typename U>
    constexpr T value_or(U&& default_value) &&
        requires ( std::is_move_constructible_v<T> && std::is_convertible_v<U&&, T> )
    {
        return has_value() ? std::move( **this ) : static_cast<T>( std::forward<U>( default_value) );
    }

    template<typename... Args>
    constexpr T& emplace( Args&&... args )
        noexcept( std::is_nothrow_constructible_v<T, Args...> )
        requires std::is_nothrow_constructible_v<T, Args...>
    {
        return storage_.template emplace<0>( std::forward<Args>( args )... );
    }

    template<typename U, typename... Args>
    constexpr T& emplace( std::initializer_list<U> il, Args&&... args )
        noexcept( std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...> )
        requires std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...>
    {
        return storage_.template emplace<0>( il, std::forward<Args>( args )... );
    }

    template<typename F>
    constexpr auto and_then( F&& f ) &
        requires std::is_constructible_v<E, E&>
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, T&>>;
        static_assert( std::is_same_v<typename U::error_type, E>, "F must return expected with same error type" );

        if ( has_value() ) {
            return std::invoke( std::forward<F>( f ), **this );
        }

        return U( std::in_place_index<1>, error() );
    }

    template<typename F>
    constexpr auto and_then( F&& f ) const&
        requires std::is_constructible_v<E, const E&>
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, const T&>>;
        static_assert( std::is_same_v<typename U::error_type, E>, "F must return expected with same error type" );

        if ( has_value() ) {
            return std::invoke( std::forward<F>( f ), **this );
        }

        return U( std::in_place_index<1>, error() );
    }

    template<typename F>
    constexpr auto and_then( F&& f ) &&
        requires std::is_constructible_v<E, E>
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, T>>;
        static_assert( std::is_same_v<typename U::error_type, E>, "F must return expected with same error type" );

        if ( has_value() ) {
            return std::invoke( std::forward<F>( f ), std::move( **this ) );
        }

        return U( std::in_place_index<1>, std::move( *this ).error() );
    }

    template<typename F>
    constexpr auto and_then( F&& f ) const&&
        requires std::is_constructible_v<E, const E>
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, const T>>;
        static_assert( std::is_same_v<typename U::error_type, E>, "F must return expected with same error type" );

        if ( has_value() ) {
            return std::invoke( std::forward<F>( f ), std::move( **this ) );
        }

        return U( std::in_place_index<1>, std::move( *this ).error() );
    }

    template<typename F>
    constexpr auto or_else( F&& f ) &
        requires std::is_constructible_v<T, T&>
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, E&>>;
        static_assert( std::is_same_v<typename G::value_type, T>, "F must return expected with same value type" );

        if ( has_value() ) {
            return G( std::in_place, **this );
        }

        return std::invoke( std::forward<F>( f ), error() );
    }

    template<typename F>
    constexpr auto or_else( F&& f ) const&
        requires std::is_constructible_v<T, const T&>
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, const E&>>;
        static_assert( std::is_same_v<typename G::value_type, T>, "F must return expected with same value type" );

        if ( has_value() ) {
            return G( std::in_place, **this );
        }

        return std::invoke( std::forward<F>( f ), error() );
    }

    template<typename F>
    constexpr auto or_else( F&& f ) &&
        requires std::is_constructible_v<T, T>
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, E>>;
        static_assert( std::is_same_v<typename G::value_type, T>, "F must return expected with same value type" );

        if ( has_value() ) {
            return G( std::in_place, std::move( **this ) );
        }

        return std::invoke( std::forward<F>( f ), std::move( *this ).error() );
    }

    template<typename F>
    constexpr auto or_else( F&& f ) const&&
        requires std::is_constructible_v<T, const T>
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, const E>>;
        static_assert( std::is_same_v<typename G::value_type, T>, "F must return expected with same value type" );

        if ( has_value() ) {
            return G( std::in_place, std::move( **this ) );
        }

        return std::invoke( std::forward<F>( f ), std::move( *this ).error() );
    }

    template<typename F>
    constexpr auto transform( F&& f ) &
        requires std::is_constructible_v<E, E&>
    {
        using U = std::remove_cv_t<std::invoke_result_t<F, T&>>;

        if ( has_value() ) {
            if constexpr ( std::is_void_v<U> ) {
                std::invoke( std::forward<F>( f ), **this );
                return expected<void, E>();
            } else {
                return expected<U, E>( std::in_place, std::invoke( std::forward<F>( f ), **this ) );
            }
        }

        return expected<U, E>( std::in_place_index<1>, error() );
    }

    template<typename F>
    constexpr auto transform( F&& f ) const&
        requires std::is_constructible_v<E, const E&>
    {
        using U = std::remove_cv_t<std::invoke_result_t<F, const T&>>;

        if ( has_value() ) {
            if constexpr ( std::is_void_v<U> ) {
                std::invoke( std::forward<F>( f ), **this );
                return expected<void, E>();
            } else {
                return expected<U, E>( std::in_place, std::invoke( std::forward<F>( f ), **this ) );
            }
        }
        return expected<U, E>( std::in_place_index<1>, error() );
    }

    template<typename F>
    constexpr auto transform( F&& f ) &&
        requires std::is_constructible_v<E, E>
    {
        using U = std::remove_cv_t<std::invoke_result_t<F, T>>;

        if ( has_value() ) {
            if constexpr ( std::is_void_v<U> ) {
                std::invoke( std::forward<F>( f ), std::move( **this ) );
                return expected<void, E>();
            } else {
                return expected<U, E>( std::in_place, std::invoke( std::forward<F>( f ), std::move( **this ) ) );
            }
        }

        return expected<U, E>( std::in_place_index<1>, std::move( *this ).error() );
    }

    template<typename F>
    constexpr auto transform( F&& f ) const&&
        requires std::is_constructible_v<E, const E>
    {
        using U = std::remove_cv_t<std::invoke_result_t<F, const T>>;

        if ( has_value() ) {
            if constexpr ( std::is_void_v<U> ) {
                std::invoke( std::forward<F>( f ), std::move( **this ) );
                return expected<void, E>();
            } else {
                return expected<U, E>( std::in_place, std::invoke( std::forward<F>( f ), std::move( **this ) ) );
            }
        }

        return expected<U, E>( std::in_place_index<1>, std::move( *this ).error() );
    }

    template<typename F>
    constexpr auto transform_error( F&& f ) &
        requires std::is_constructible_v<T, T&>
    {
        using G = std::remove_cv_t<std::invoke_result_t<F, E&>>;

        if ( has_value() ) {
            return expected<T, G>( std::in_place, **this );
        }

        return expected<T, G>( std::in_place_index<1>, std::invoke( std::forward<F>( f ), error() ) );
    }

    template<typename F>
    constexpr auto transform_error( F&& f ) const&
        requires std::is_constructible_v<T, const T&>
    {
        using G = std::remove_cv_t<std::invoke_result_t<F, const E&>>;

        if ( has_value() ) {
            return expected<T, G>( std::in_place, **this );
        }

        return expected<T, G>( std::in_place_index<1>, std::invoke( std::forward<F>( f ), error() ) );
    }

    template<typename F>
    constexpr auto transform_error( F&& f ) &&
        requires std::is_constructible_v<T, T>
    {
        using G = std::remove_cv_t<std::invoke_result_t<F, E>>;

        if ( has_value() ) {
            return expected<T, G>( std::in_place, std::move( **this ) );
        }

        return expected<T, G>( std::in_place_index<1>, std::invoke( std::forward<F>( f ), std::move( *this ).error() ) );
    }

    template<typename F>
    constexpr auto transform_error( F&& f ) const&&
        requires std::is_constructible_v<T, const T>
    {
        using G = std::remove_cv_t<std::invoke_result_t<F, const E>>;

        if ( has_value() ) {
            return expected<T, G>( std::in_place, std::move( **this ) );
        }

        return expected<T, G>( std::in_place_index<1>, std::invoke( std::forward<F>( f ), std::move( *this ).error() ) );
    }

    constexpr void swap( expected& other )
        noexcept( std::is_nothrow_move_constructible_v<T> &&
                 std::is_nothrow_swappable_v<T> &&
                 std::is_nothrow_move_constructible_v<E> &&
                 std::is_nothrow_swappable_v<E> )
        requires ( std::is_swappable_v<T> && std::is_swappable_v<E> &&
                  std::is_move_constructible_v<T> && std::is_move_constructible_v<E> &&
                  ( std::is_nothrow_move_constructible_v<T> || std::is_nothrow_move_constructible_v<E> ) )
    {
        using std::swap;
        swap( storage_, other.storage_ );
    }

    template<typename T2, typename E2>
    friend constexpr bool operator==( const expected& x, const expected<T2, E2>& y ) {
        if ( x.has_value() != y.has_value() ) {
            return false;
        }
        if ( x.has_value() ) {
            return *x == *y;
        }
        return x.error() == y.error();
    }

    template<typename T2>
    friend constexpr bool operator==( const expected& x, const T2& v ) {
        return x.has_value() && *x == v;
    }

    template<typename E2>
    friend constexpr bool operator==( const expected& x, const unexpected<E2>& e ) {
        return !x.has_value() && x.error() == e.error();
    }
};

template<typename T, typename E>
constexpr void swap( expected<T, E>& lhs, expected<T, E>& rhs )
    noexcept( noexcept ( lhs.swap( rhs )) )
    requires requires { lhs.swap( rhs ); }
{
    lhs.swap( rhs );
}

template<typename E>
constexpr void swap( unexpected<E>& lhs, unexpected<E>& rhs )
    noexcept( noexcept( lhs.swap( rhs ) ) )
    requires requires { lhs.swap( rhs ); }
{
    lhs.swap(rhs);
}

template<typename E>
constexpr unexpected<std::decay_t<E>> make_unexpected( E&& e ) {
    return unexpected<std::decay_t<E>>( std::forward<E>( e ) );
}

}

#endif //STL_EXPECTED_HPP