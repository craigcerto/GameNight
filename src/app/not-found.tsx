export default function NotFound() {
  return (
    <div className="flex flex-col items-center justify-center min-h-[60vh] text-center">
      <h2 className="text-4xl font-bold mb-4">404 - Page Not Found</h2>
      <p className="text-muted-foreground mb-8">
        The page you're looking for doesn't exist.
      </p>
      <a href="/" className="text-primary underline">
        Go back home
      </a>
    </div>
  )
}
