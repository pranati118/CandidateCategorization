# Candidate Categorization Dashboard

An interactive R Shiny application for clustering and analyzing job applicant data. This tool allows **job seekers** to submit their profiles (either manually or via resume parsing), and enables **employers** to explore clustering of candidates using machine learning techniques.

---

## ✨ Features

### 🧑‍💼 Job Seeker Module

- **Manual Entry**: Users can directly enter their details such as name, experience, and skills.
- **Resume Upload**: Automatically extracts relevant information (e.g., skills, experience) using resume parsing.
- All details are stored in a central **SQLite database** for further analysis.

### 🏢 Employer Module

- Choose from multiple **clustering algorithms**:
  - ✅ K-Means
  - ✅ Hierarchical Clustering
  - 🚧 (Optional future addition) DBSCAN / GMM
- Visualize clusters of candidates based on:
  - **Experience**
  - **Skill Count**
- View and compare clustering results using:
  - **Silhouette Score**
  - **WSS (Within-Cluster Sum of Squares)**
- Suggested number of clusters (K) based on both evaluation metrics.

---

