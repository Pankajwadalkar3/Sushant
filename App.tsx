import React, { useState } from 'react';
import { AppState, TransformationTarget, CobolFile, AnalysisResult, ProjectStructure } from './types';
import UploadSection from './components/UploadSection';
import DatapointReview from './components/DatapointReview';
import TransformationResult from './components/TransformationResult';
import { extractCobolFiles } from './services/zipService';
import { analyzeCobolCode, generateModernArchitecture } from './services/geminiService';
import { Activity, Cpu, FileCode } from 'lucide-react';

const App: React.FC = () => {
  const [state, setState] = useState<AppState>(AppState.UPLOAD);
  const [target, setTarget] = useState<TransformationTarget>(TransformationTarget.DOTNET);
  const [files, setFiles] = useState<CobolFile[]>([]);
  const [analysis, setAnalysis] = useState<AnalysisResult | null>(null);
  const [project, setProject] = useState<ProjectStructure | null>(null);
  const [error, setError] = useState<string | null>(null);

  const handleUpload = async (fileList: FileList) => {
    setError(null);
    setState(AppState.ANALYZING);
    
    try {
      // 1. Extract Files
      const extractedFiles = await extractCobolFiles(fileList[0]);
      setFiles(extractedFiles);

      // 2. Analyze with Gemini
      const analysisResult = await analyzeCobolCode(extractedFiles);
      setAnalysis(analysisResult);
      
      setState(AppState.REVIEW);
    } catch (err) {
      console.error(err);
      setError("Failed to process files. Please ensure you are using a valid Zip or CBL file.");
      setState(AppState.UPLOAD);
    }
  };

  const handleTransformation = async () => {
    if (!analysis || files.length === 0) return;
    
    setState(AppState.TRANSFORMING);
    try {
      const result = await generateModernArchitecture(files, analysis, target);
      setProject(result);
      setState(AppState.COMPLETE);
    } catch (err) {
      setError("Transformation failed. Please try again.");
      setState(AppState.REVIEW);
    }
  };

  return (
    <div className="min-h-screen bg-slate-50 font-sans text-slate-900">
      {/* Header */}
      <header className="bg-white border-b border-slate-200 sticky top-0 z-50">
        <div className="max-w-7xl mx-auto px-4 h-16 flex items-center justify-between">
            <div className="flex items-center gap-2">
                <div className="w-8 h-8 bg-indigo-600 rounded-lg flex items-center justify-center">
                    <Cpu className="w-5 h-5 text-white" />
                </div>
                <span className="font-bold text-xl tracking-tight">LegacyForge</span>
            </div>
            <div className="flex items-center gap-4">
                <div className="text-sm text-slate-500">
                  API Status: <span className="text-green-600 font-medium">Online</span>
                </div>
            </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 py-12 h-[calc(100vh-64px)]">
        {error && (
            <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg text-red-700 flex items-center gap-2">
                <Activity className="w-5 h-5" />
                {error}
            </div>
        )}

        {state === AppState.UPLOAD && (
            <UploadSection 
                onUpload={handleUpload} 
                target={target} 
                setTarget={setTarget} 
                isProcessing={false} 
            />
        )}

        {state === AppState.ANALYZING && (
            <div className="flex flex-col items-center justify-center h-[60vh] space-y-6 animate-fade-in">
                <div className="relative">
                    <div className="w-24 h-24 border-4 border-slate-200 border-t-indigo-600 rounded-full animate-spin"></div>
                    <div className="absolute inset-0 flex items-center justify-center">
                        <Cpu className="w-8 h-8 text-indigo-600" />
                    </div>
                </div>
                <div className="text-center">
                    <h3 className="text-xl font-semibold text-slate-900">Analyzing Source Code</h3>
                    <p className="text-slate-500 mt-2">Identifying entities, logic patterns, and dependencies...</p>
                </div>
            </div>
        )}

        {state === AppState.REVIEW && analysis && (
            <DatapointReview 
                analysis={analysis} 
                onConfirm={handleTransformation} 
                onBack={() => setState(AppState.UPLOAD)}
            />
        )}

        {state === AppState.TRANSFORMING && (
            <div className="flex flex-col items-center justify-center h-[60vh] space-y-6 animate-fade-in">
                <div className="relative">
                    <div className="w-24 h-24 border-4 border-slate-200 border-t-green-600 rounded-full animate-spin"></div>
                    <div className="absolute inset-0 flex items-center justify-center">
                        <FileCode className="w-8 h-8 text-green-600" />
                    </div>
                </div>
                <div className="text-center">
                    <h3 className="text-xl font-semibold text-slate-900">Generating Architecture</h3>
                    <p className="text-slate-500 mt-2">Writing Clean Architecture components for {target}...</p>
                </div>
            </div>
        )}

        {state === AppState.COMPLETE && project && (
            <TransformationResult 
                project={project} 
                onReset={() => setState(AppState.UPLOAD)} 
            />
        )}
      </main>
    </div>
  );
};

export default App;