interface NavigationBasics {
  color?: any
  textColor?: any
  group?: string
  items?: Navigations
  item?: string
  href?: string
  icon?: string
  iconColor?: any
  divider?: string
}
type RequireAtLeastOne<T, Keys extends keyof T = keyof T> = Pick<
  T,
  Exclude<keyof T, Keys>
> &
  {
    [K in Keys]-?: Required<Pick<T, K>> & Partial<Pick<T, Exclude<Keys, K>>>
  }[Keys]
export type Navigation = RequireAtLeastOne<
  NavigationBasics,
  "group" | "item" | "divider"
>
export type Navigations = Navigation[]
